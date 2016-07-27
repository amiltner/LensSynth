open Core.Std
open Lens
open Fasteval
open Regexcontext
open Gen
open Lang
open Util

let create_userdef (c:RegexContext.t)
                   (userdef_name:string)
                   (userdef_meaning:regex)
                   (concrete:bool)
                   : (RegexContext.t) =
  let rec check_welldefined_regex (r:regex) : unit =
    begin match r with
    | RegExEmpty -> ()
    | RegExMappedUserDefined _ -> failwith "THIS HSOULDNT HAPPEN AH"
    | RegExBase _ -> ()
    | RegExConcat (r1,r2) -> 
        check_welldefined_regex r1;
        check_welldefined_regex r2
    | RegExOr (r1,r2) ->
        check_welldefined_regex r1;
        check_welldefined_regex r2
    | RegExStar r' -> check_welldefined_regex r'
    | RegExUserDefined t ->
      let _ = RegexContext.lookup_exn c t in
      ()
    end
  in
  check_welldefined_regex userdef_meaning;
  (RegexContext.insert_exn c userdef_name userdef_meaning (not concrete))

let test_string (showing_strategy:string -> unit)
                (c:RegexContext.t)
                (r:regex)
                (s:string)
                : unit =
  let evaluation_result_string =
    (if fast_eval c [] r s then
      (s ^ " matches regular expression " ^ (Pp.pp_regexp r))
    else
      (s ^ " does not match regular expression " ^ (Pp.pp_regexp r)))
  in
  showing_strategy evaluation_result_string

let synthesize_lens (lens_handler:(dnf_lens*regex*regex*RegexContext.t) option -> regex ->
  regex -> examples -> RegexContext.t -> unit)
                    (c:RegexContext.t)
                    ((name,r1,r2,exs):specification)
                    (iteratively_deepen_strategy:bool)
                    : unit =
  let dro = gen_dnf_lens c r1 r2 exs iteratively_deepen_strategy in
  (*let lens_string =
    "\n\n" ^ name ^ ": " ^
      (begin match lens_option with
      | None -> "no lens found"
      | Some ls -> Pp.pp_dnf_lens ls
      end) in
  showing_strategy lens_string*)
  lens_handler dro r1 r2 exs c

let run_declaration (lens_handler:(dnf_lens*regex*regex*RegexContext.t) option -> regex ->
  regex -> examples -> RegexContext.t -> unit)
                    (test_handler:string -> unit)
                    (c:RegexContext.t)
                    (d:declaration)
                    (iteratively_deepen_strategy:bool)
                    : (RegexContext.t) =
  begin match d with
  | DeclUserdefCreation (s,r,b) -> create_userdef c s r b
  | DeclTestString (r,s) -> (test_string test_handler c r s); (c)
  | DeclSynthesizeProgram spec ->
      synthesize_lens lens_handler c spec iteratively_deepen_strategy;
      c
  end

let run_declarations (lens_handler:(dnf_lens*regex*regex*RegexContext.t) option -> regex ->
  regex -> examples -> RegexContext.t -> unit) (test_handler:string -> unit)(ds:declaration list) (iteratively_deepen_strategy:bool) : unit =
  let _ = (List.fold_left
    ~f:(fun acc d -> run_declaration lens_handler test_handler acc d iteratively_deepen_strategy)
    ~init:RegexContext.empty
    ds) in
  ()
