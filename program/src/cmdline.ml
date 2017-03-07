open Util
open Pp
open Lens_put
open Regexcontext
open Lenscontext
open Gen_exs
open Gen
open Core.Std
open Lang
open Consts
open Typing
open Program_utilities
open Boom_lang
open Eval
open Converter
open Normalized_lang
open Regex_utilities
open Language_equivalences

let _ = Random.self_init

exception Arg_exception

type driver_mode =
  | Default
  | Parse
  | Synth
  | Time
  | GeneratedExamples
  | MaxSpecify
  | SpecSize

let usage_msg = "synml [-help | opts...] <src>"
let filename : string option ref = ref None
let mode : driver_mode ref = ref Default

let parse_file (f:string) : program =
  Preproc.preprocess_file f
    |> Lexing.from_string
    |> Parser.program Lexer.token

let set_opt (d:driver_mode) =
  match !mode with
  | Default -> mode := d
  | _ -> raise Arg_exception

let args =
  [ ( "-parse"
    , Arg.Unit (fun _ -> set_opt Parse)
    , " Parse only"
    )
  ; ( "-synth"
    , Arg.Unit (fun _ -> set_opt Synth)
    , " Synthesize"
    )
  ; ( "-time"
    , Arg.Unit (fun _ -> set_opt Time)
    , " Synthesize and collect statistics (time)"
    )
  ; ( "-generatedexamples"
    , Arg.Unit (fun _ -> set_opt GeneratedExamples)
    , " Synthesize with randomly generated examples"
    )
  ; ( "-forceexpand"
    , Arg.Unit (fun _ -> force_expand_regexps := true)
    , " Synthesize with definitions forced to be expanded"
    )
  ; ( "-iterativedeepenstrategy"
    , Arg.Unit (fun _ -> use_iterative_deepen_strategy := true)
    , " Set use of iterative deepen strategy"
    )
  ; ( "-max_to_specify"
    , Arg.Unit (fun _ -> set_opt MaxSpecify)
    , " Set to calculate the maximum number of examples to fully specify"
    )
  ; ( "-spec_size"
    , Arg.Unit (fun _ -> set_opt SpecSize)
    , " Set to calculate the size of the user input specification size"
    )
  ; ( "-naive_strategy"
    , Arg.Unit (fun _ -> naive_strategy := true)
    , " Set to use a naive synthesis strategy"
    )
  ; ( "-naive_pqueue"
    , Arg.Unit (fun _ -> naive_pqueue := true)
    , " Set to use a naive priority queue"
    )
  ; ( "-no_short_circuit"
    , Arg.Unit (fun _ -> short_circuit := false)
    , " Short circuit when distance is 0"
    )
  ; ( "-no_lens_context"
    , Arg.Unit (fun _ -> use_lens_context := false)
    , " Short circuit when distance is 0"
    )
  ; ( "-verbose"
    , Arg.Unit (fun _ -> verbose := true)
    , " Print out information about synthesis"
    )
  ]
  |> Arg.align

let expand_regexps (p:program) : program =
  let rec expand_regexp (r:regex) (c:RegexContext.t) : regex =
    begin match r with
    | RegExEmpty -> r
    | RegExBase _ -> r
    | RegExConcat (r1,r2) -> RegExConcat (expand_regexp r1 c,expand_regexp r2 c)
    | RegExOr (r1,r2) -> RegExOr (expand_regexp r1 c,expand_regexp r2 c)
    | RegExStar r' -> RegExStar (expand_regexp r' c)
    | RegExVariable t ->
      let r' = RegexContext.lookup_exn c t in
      expand_regexp r' c
    end
  in
  let expand_regexps_decl (c:RegexContext.t)
                          (d:declaration)
                          : (RegexContext.t * declaration) =
    begin match d with
    | DeclRegexCreation (s,r,b) -> (RegexContext.insert_exn c s r b,d)
    | DeclTestString _ -> (c,d)
    | DeclSynthesizeLens (n,r1,r2,exs) ->
      (c,DeclSynthesizeLens (n,expand_regexp r1 c,expand_regexp r2 c,exs))
    | DeclLensCreation _ -> (c,d)
    | DeclTestLens _ -> (c,d)
    end
  in
  List.rev
  (snd
    (List.fold_left
      ~f:(fun (c,ds) d ->
        let (c,d') = expand_regexps_decl c d in
        (c,d'::ds))
      ~init:(RegexContext.empty,[])
      p))

let expand_regexps_if_necessary (p:program) : program =
  if !force_expand_regexps then
    expand_regexps p
  else
    p

let rec check_examples
    (rc:RegexContext.t)
    (lc:LensContext.t)
    (l:lens)
    (exs:examples)
  : unit =
  begin match exs with
    | [] -> ()
    | (lex,rex)::exst ->
      if lens_putr rc lc l lex <> rex then
        failwith "put right didnt work"
      else if lens_putl rc lc l rex <> lex then
        failwith "put left didnt work"
      else
        check_examples rc lc l exst
  end

let print_lenses (lc:LensContext.t) (lss:(string * lens option) list) : unit =
  List.iter
    ~f:(fun (s,lo) ->
      print_endline "\n\n";
      print_endline (s ^ ":");
      begin match lo with
      | None -> print_endline "no lens found"
      | Some ls -> print_endline (lens_to_string ls);
        let (t1,t2) = type_lens lc ls in
        print_endline (regex_to_string t1);
        print_endline (regex_to_string t2)
      end)
    lss

let ignore (_:'a) : unit =
  ()


let collect_time (p:program) : unit =
  let (time, _) = Util.time_action ~f:(fun _ -> synthesize_program p) in
  print_endline (Float.to_string time)

let collect_example_number (p:program) : unit =
  let current_total = ref 0 in
  let cb = fun (rc,lc,r1,r2,exs) ->
    let l = Option.value_exn (gen_lens rc lc r1 r2 exs) in
    let exs_required = fold_until_completion
        (fun acc ->
           let l' = Option.value_exn
               (gen_lens rc lc r1 r2 acc) in
           if l' = l then
             Right (List.length acc)
           else
             let leftex = gen_element_of_dnf_regex rc (to_dnf_regex r1) in
             let rightex = lens_putr rc lc l leftex in
             let acc = (leftex,rightex)::acc in
             Left acc
        ) [] in
    current_total := !current_total + exs_required;
  in
  let _ = synthesize_and_load_program_with_callback
    p
    cb
  in
  print_endline (string_of_int !current_total)

let rec max_examples_required
    (rc:RegexContext.t)
    (lc:LensContext.t)
    (l:lens)
  : int =
  let sum_list = List.fold_left ~f:(+) ~init:0 in
  let rec examples_required_o_ex_dnf_regex (o_ex_dr:ordered_exampled_dnf_regex) : int =
    let seq_list_list =
      List.map
        ~f:(fun l -> List.map ~f:fst l)
        o_ex_dr
    in
    let examples_required_list_list =
      List.map
        ~f:(fun l -> List.map ~f:examples_required_o_ex_sequence l)
        seq_list_list
    in
    let total_examples_list =
      List.map
        ~f:(fun l -> (List.length l - 1) + sum_list l)
        examples_required_list_list
    in
    sum_list total_examples_list
  and examples_required_o_ex_sequence (o_ex_sq:ordered_exampled_clause) : int =
    let (o_ex_a_i_list_list,_,_) = o_ex_sq in
    let atom_list_list =
      List.map
        ~f:(fun l -> List.map ~f:fst l)
        o_ex_a_i_list_list
    in
    let examples_required_list_list =
      List.map
        ~f:(fun l -> List.map ~f:examples_required_o_ex_atom l)
        atom_list_list
    in
    let total_examples_list =
      List.map
        ~f:(fun l -> (List.length l - 1) + sum_list l)
        examples_required_list_list
    in
    sum_list total_examples_list
  and examples_required_o_ex_atom (_:ordered_exampled_atom) : int =
    1
  in
  let rec referenced_lenses (l:lens) : lens list =
    begin match l with
      | LensConst _ -> []
      | LensConcat (l1,l2) -> (referenced_lenses l1) @ (referenced_lenses l2)
      | LensSwap (l1,l2) -> (referenced_lenses l1) @ (referenced_lenses l2)
      | LensUnion (l1,l2) -> (referenced_lenses l1) @ (referenced_lenses l2)
      | LensCompose (l1,l2) -> (referenced_lenses l1) @ (referenced_lenses l2)
      | LensIterate (l') -> (referenced_lenses l')
      | LensIdentity _ -> []
      | LensInverse l' -> (referenced_lenses l')
      | LensVariable v ->
        [LensContext.lookup_impl_exn lc v]
      | LensPermute (_,ll) ->
        List.concat_map ~f:referenced_lenses ll
    end
  in
  let (r,_) = type_lens lc l in
  let ex_r = Option.value_exn (regex_to_exampled_regex rc r []) in
  let ex_dr = exampled_regex_to_exampled_dnf_regex rc lc ex_r in
  let o_ex_dr = to_ordered_exampled_dnf_regex ex_dr in
  let ref_lenses = referenced_lenses l in
  let ref_lenses_exs = List.map ~f:(max_examples_required rc lc) ref_lenses in
  (examples_required_o_ex_dnf_regex o_ex_dr) + (sum_list ref_lenses_exs)
    
let collect_max_example_number (p:program) : unit =
  let (rc,lc,r1,r2,exs) = retrieve_last_synthesis_problem_exn p in
  let l = Option.value_exn (gen_lens rc lc r1 r2 exs) in
  let max_exs_required = max_examples_required rc lc l in
  print_endline (string_of_int max_exs_required)

let specification_size (p:program) : unit =
  let (rc,_,r1,r2,_) = retrieve_last_synthesis_problem_exn p in
  let rec retrieve_transitive_userdefs (r:regex) : string list =
    begin match r with
    | RegExEmpty -> []
    | RegExBase _ -> []
    | RegExConcat (r1,r2) -> (retrieve_transitive_userdefs r1) @
        (retrieve_transitive_userdefs r2)
    | RegExOr (r1,r2) -> (retrieve_transitive_userdefs r1) @
        (retrieve_transitive_userdefs r2)
    | RegExStar r' -> retrieve_transitive_userdefs r'
    | RegExVariable t ->
      t::(begin match RegexContext.lookup_for_expansion_exn rc t with
      | None -> []
      | Some rex -> retrieve_transitive_userdefs rex
      end)
    end
  in
  let all_userdefs = (retrieve_transitive_userdefs r1) @
                     (retrieve_transitive_userdefs r2) in
  let all_regexps =
    List.dedup(
      r1::r2::(List.map ~f:(fun s ->
          (RegexContext.lookup_exn rc s))
          all_userdefs))
  in
  let all_sizes =
    List.map ~f:(fun r -> size r) all_regexps
  in
  let mysize = List.fold_left
    ~f:(+)
    ~init:0
    all_sizes
  in

  print_endline (string_of_int (mysize))

let print_outputs (p:program) : unit =
  print_endline "module Generated =";
  let (_,lc,p) = synthesize_and_load_program p in
  let bp = boom_program_of_program lc p in
  print_endline (pp_program bp)

let main () =
  begin try
    Arg.parse args (fun s ->
      match !filename with
      | Some _ -> raise Arg_exception
      | None -> filename := Some s) usage_msg
  with
    Arg_exception -> Arg.usage args usage_msg
  end;
  match !filename with
  | None   -> Arg.usage args usage_msg
  | Some f ->
    begin match Sys.file_exists f with
      | `No | `Unknown -> Arg.usage args ("File not found: " ^ f)
      | `Yes -> begin match !mode with
          | Parse ->
            let _ = parse_file f in (*TODO: pp*) ()(*Printf.printf "%s\n"
                                                     (Pp.pp_prog prog)*)
          | Time -> parse_file f |> expand_regexps_if_necessary |> collect_time
          | GeneratedExamples ->
            parse_file f
            |> expand_regexps_if_necessary
            |> collect_example_number
          | MaxSpecify ->
            parse_file f
            |> expand_regexps_if_necessary
            |> collect_max_example_number
          | Default | Synth ->
            parse_file f |> expand_regexps_if_necessary |> print_outputs
          | SpecSize ->
            parse_file f |> expand_regexps_if_necessary |> specification_size
        end
    end

let () = if not !Sys.interactive then main ()
