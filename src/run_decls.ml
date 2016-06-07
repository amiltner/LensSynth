open Core.Std
open Lens
open Fasteval
open Gen
open Lang
open Util

let create_userdef (c:context)
                   (e_c:evaluation_context)
                   (userdef_name:string)
                   (userdef_meaning:regex)
                   (concrete:bool)
                   : (context * evaluation_context) =
  let rec check_welldefined_regex (r:regex) : unit =
    begin match r with
    | RegExBase _ -> ()
    | RegExConcat (r1,r2) -> 
        check_welldefined_regex r1;
        check_welldefined_regex r2
    | RegExOr (r1,r2) ->
        check_welldefined_regex r1;
        check_welldefined_regex r2
    | RegExStar r' -> check_welldefined_regex r'
    | RegExUserDefined t ->
        if not (List.Assoc.mem e_c t) then
          failwith (userdef_name ^ " ill defined, has undefined userdef " ^ t)
        else
          ()
    end
  in
  check_welldefined_regex userdef_meaning;
  (if (List.Assoc.mem e_c userdef_name) then
    failwith ("multiply defined " ^ userdef_name)
  else
    ());
  (
    (if concrete then
      (userdef_name,userdef_meaning)::c
    else
      c),
  (userdef_name,userdef_meaning)::e_c)

let test_string (showing_strategy:string -> unit)
                (e_c:evaluation_context)
                (r:regex)
                (s:string)
                : unit =
  let evaluation_result_string =
    (if fast_eval e_c r s then
      (s ^ " matches regular expression " ^ (Pp.pp_regexp r))
    else
      (s ^ " does not match regular expression " ^ (Pp.pp_regexp r)))
  in
  showing_strategy evaluation_result_string

let synthesize_lens (lens_handler:(dnf_lens*regex*regex) option -> regex ->
  regex -> context -> evaluation_context -> unit)
                    (c:context)
                    (e_c:evaluation_context)
                    ((name,r1,r2,exs):specification)
                    : unit =
  let dro = gen_dnf_lens c e_c r1 r2 exs in
  (*let lens_string =
    "\n\n" ^ name ^ ": " ^
      (begin match lens_option with
      | None -> "no lens found"
      | Some ls -> Pp.pp_dnf_lens ls
      end) in
  showing_strategy lens_string*)
  lens_handler dro r1 r2 c e_c

let run_declaration (lens_handler:(dnf_lens*regex*regex) option -> regex ->
  regex -> context -> evaluation_context -> unit)
                    (test_handler:string -> unit)
                    ((c,e_c):context * evaluation_context)
                    (d:declaration)
                    : (context * evaluation_context) =
  begin match d with
  | DeclUserdefCreation (s,r,b) -> create_userdef c e_c s r b
  | DeclTestString (r,s) -> (test_string test_handler e_c r s); (c,e_c)
  | DeclSynthesizeProgram spec ->
      synthesize_lens lens_handler c e_c spec;
      (c,e_c)
  end

let run_declarations (lens_handler:(dnf_lens*regex*regex) option -> regex ->
  regex -> context ->
  evaluation_context -> unit) (test_handler:string -> unit)(ds:declaration list) : unit =
  let _ = (List.fold_left
    ~f:(fun acc d -> run_declaration lens_handler test_handler acc d)
    ~init:([],[])
    ds) in
  ()
