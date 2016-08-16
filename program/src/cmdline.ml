open Util
open Pp_as_boom
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

exception Arg_exception

type driver_mode =
  | Default
  | Parse
  | Synth
  | Time
  | GeneratedExamples

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
    , Arg.Unit (fun _ -> force_expand_regexps := false)
    , " Synthesize with definitions forced to be expanded"
    )
  ; ( "-iterativedeepenstrategy"
    , Arg.Unit (fun _ -> use_iterative_deepen_strategy := true)
    , " Set use of iterative deepen strategy"
    )
  ]
  |> Arg.align

let expand_regexps (p:program) : program =
  let rec expand_regexp (r:regex) (c:RegexContext.t) : regex =
    print_endline (boom_pp_regex (RegExBase "T"));
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
      | Some ls -> print_endline (string_of_lens ls);
        let (t1,t2) = type_lens lc ls in
        print_endline (string_of_regex t1);
        print_endline (string_of_regex t2)
      end)
    lss

let ignore (_:'a) : unit =
  ()


let collect_time (p:program) : unit =
  let (time, _) = Util.time_action ~f:(fun _ -> synthesize_program p) in
  print_endline (Float.to_string time)

let collect_example_number (p:program) : unit =
  let (rc,lc,r1,r2,exs) = retrieve_last_synthesis_problem_exn p in
  let l = Option.value_exn (gen_lens rc lc r1 r2 exs) in
  let exs_required = fold_until_completion
      (fun acc ->
         let l' = Option.value_exn
             (gen_lens rc lc r1 r2 acc) in
         if l' = l then
           Right (List.length acc)
         else
           let leftex = gen_element_of_regex_language rc r1 in
           let rightex = lens_putr rc lc l leftex in
           let acc = (leftex,rightex)::acc in
           Left acc
      ) [] in
  print_endline (string_of_int exs_required)

let print_outputs (p:program) : unit =
  print_endline "module Generated =";
  let p = synthesize_program p in
  let bp = boom_program_of_program p in
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
            parse_file f |> expand_regexps_if_necessary |> collect_example_number
          | Default | Synth ->
            parse_file f |> expand_regexps_if_necessary |> print_outputs
        end
    end

let () = if not !Sys.interactive then main ()
