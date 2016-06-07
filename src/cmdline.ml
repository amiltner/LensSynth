open Lang
open Util
open Lens_put
open Gen_exs
open Transform
open Run_decls
open Pp
open Gen
open Lens
open Core.Std
open Consts
open Lang
open Consts

exception Arg_exception

type driver_mode =
  | Default
  | Parse
  | Data
  | Synth
  | ForceExpand

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
  ; ( "-data"
    , Arg.Unit (fun _ -> set_opt Data)
    , " Synthesize and collect statistics (name, #/exs, size, time)"
    )
  ; ( "-forceexpand"
    , Arg.Unit (fun _ -> set_opt ForceExpand)
    , " Synthesize with definitions forced to be expanded"
    )
  ]
  |> Arg.align

let expand_regexps (p:program) : program =
  let rec expand_regexp (r:regex) (e_c:evaluation_context) : regex =
    begin match r with
    | RegExBase _ -> r
    | RegExConcat (r1,r2) -> RegExConcat (expand_regexp r1 e_c,expand_regexp r2 e_c)
    | RegExOr (r1,r2) -> RegExOr (expand_regexp r1 e_c,expand_regexp r2 e_c)
    | RegExStar r' -> RegExStar (expand_regexp r' e_c)
    | RegExUserDefined t ->
      begin match List.Assoc.find e_c t with
      | Some r' -> expand_regexp r' e_c
      | None -> failwith (string_of_int (List.length e_c))
      end
    end
  in
  let expand_regexps_decl (e_c:evaluation_context)
                          (d:declaration)
                          : (evaluation_context * declaration) =
    begin match d with
    | DeclUserdefCreation (s,r,b) -> ((s,r)::e_c,d)
    | DeclTestString _ -> (e_c,d)
    | DeclSynthesizeProgram (n,r1,r2,exs) ->
        (e_c,DeclSynthesizeProgram (n,expand_regexp r1 e_c,expand_regexp r2 e_c,exs))
    end
  in
  (snd
    (List.fold_left
      ~f:(fun (e_c,ds) d ->
        let (e_c',d') = expand_regexps_decl e_c d in
        (e_c',d'::ds))
      ~init:([],[])
      p))

let print_lenses (lss:(string * lens option) list) : unit =
  List.iter
    ~f:(fun (s,lo) ->
      print_endline "\n\n";
      print_endline (s ^ ":");
      begin match lo with
      | None -> print_endline "no lens found"
      | Some ls -> print_endline (Pp.pp_lens ls)
      end)
    lss

let ignore (x:'a) : unit =
  ()

let synthesize_prog (ps:synth_problems) : (string * lens option) list =
  let problem_list = problems_to_problem_list ps in
  List.map
    ~f:(fun (c,e_c,n,r1,r2,exs) -> (n,(gen_lens c e_c r1 
    r2 exs)))
    problem_list
  
  (*let (s, g, env, x, t, es, vs, tree) = process_preamble p in
  begin match Synth.synthesize s env tree with
  | Some e ->
      Printf.printf "%s\n" (Translate.to_top_level x t e |> Pp.pp_decl)
  | None -> begin
      Printf.printf "No expression found!\n";
      Printf.printf "final rtree size = %d\n" (Rtree.rtree_size tree)
    end
  end;
  pTODO*)

let collect_data (include_counts:bool) (p:program) : unit =
  let (time, _) = Util.time_action (fun _ ->
    run_declarations (fun _ _ _ _ _ -> ()) (fun _ -> ()) p)
  in

  if include_counts then
    (let num_examples = ref 0 in
    run_declarations (fun drro r1 r2 c e_c ->
      let (l,r1',r2') = Option.value_exn drro in
      let exs_reqd = fold_until_completion
          (fun acc ->
            let (l',_,_) = Option.value_exn
              (gen_dnf_lens_zipper c e_c r1 r2 acc) in
            if l' = l then
              Right acc
            else
              let leftex = gen_element_of_regex_language e_c r1 in
              let rightex = dnf_lens_putr e_c r1' l leftex in
              let acc = (leftex,rightex)::acc in
              Left acc
          ) [] in
      num_examples := List.length exs_reqd
      ) (fun _ -> ()) p;
    print_endline (Float.to_string time ^ "," ^ (string_of_int !num_examples))
    )
  else
    print_endline (Float.to_string time)

let print_outputs (p:program) : unit =
  run_declarations
    (fun (drro:(dnf_lens*regex*regex) option) _ _ _ _ ->
      begin match drro with
      | Some (d,_,_) -> print_endline (Pp.pp_lens (dnf_lens_to_lens d))
      | None -> print_endline "no lens found"
      end)
    print_endline p

let main () =
  begin try
    Arg.parse args (fun s ->
      match !filename with
      | Some f -> raise Arg_exception
      | None -> filename := Some s) usage_msg
  with
    Arg_exception -> Arg.usage args usage_msg
  end;
  match !filename with
  | None   -> Arg.usage args usage_msg
  | Some f ->
    print_time_if_timing begin fun _ ->
      begin match Sys.file_exists f with
      | `No | `Unknown -> Arg.usage args ("File not found: " ^ f)
      | `Yes -> begin match !mode with
        | Parse ->
            let _ = parse_file f in (*TODO: pp*) ()(*Printf.printf "%s\n"
            (Pp.pp_prog prog)*)
        | Data -> parse_file f |> (collect_data true)
        | Default | Synth ->
            parse_file f |> print_outputs
        | ForceExpand ->
            parse_file f |> expand_regexps |> (collect_data true)
        end
      end
    end

let () = if not !Sys.interactive then main ()
