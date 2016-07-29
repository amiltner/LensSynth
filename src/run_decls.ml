open Core.Std
open Lens
open Regexcontext
open Lenscontext
open Regex
open Eval
open Gen
open Lang
open Util
open Transform

let create_userdef (c:RegexContext.t)
                   (userdef_name:string)
                   (userdef_meaning:regex)
                   (concrete:bool)
                   : (RegexContext.t) =
  let rec check_welldefined_regex (r:regex) : unit =
    begin match r with
    | RegExEmpty -> ()
    | RegExMapped _ -> failwith "THIS HSOULDNT HAPPEN AH"
    | RegExBase _ -> ()
    | RegExConcat (r1,r2) -> 
        check_welldefined_regex r1;
        check_welldefined_regex r2
    | RegExOr (r1,r2) ->
        check_welldefined_regex r1;
        check_welldefined_regex r2
    | RegExStar r' -> check_welldefined_regex r'
    | RegExVariable t ->
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
  regex -> examples -> RegexContext.t -> LensContext.t -> unit)
    (rc:RegexContext.t)
    (lc:LensContext.t)
                    ((name,r1,r2,exs):specification)
                    (iteratively_deepen_strategy:bool)
                    : lens option =
  let dro = gen_dnf_lens rc lc r1 r2 exs iteratively_deepen_strategy in
  (*let lens_string =
    "\n\n" ^ name ^ ": " ^
      (begin match lens_option with
      | None -> "no lens found"
      | Some ls -> Pp.pp_dnf_lens ls
      end) in
  showing_strategy lens_string*)
  lens_handler dro r1 r2 exs rc lc;
  Option.map ~f:(fun (dl,_,_,_) -> dnf_lens_to_lens dl) dro

let run_declaration (lens_handler:(dnf_lens*regex*regex*RegexContext.t) option -> regex ->
  regex -> examples -> RegexContext.t -> LensContext.t -> unit)
                    (test_handler:string -> unit)
                    (rc:RegexContext.t)
                    (lc:LensContext.t)
                    (d:declaration)
                    (iteratively_deepen_strategy:bool)
                    : (RegexContext.t * LensContext.t) =
  begin match d with
  | DeclUserdefCreation (s,r,b) -> (create_userdef rc s r b, lc)
  | DeclTestString (r,s) -> (test_string test_handler rc r s); (rc,lc)
  | DeclSynthesizeProgram (name,r1,r2,exs) ->
    let lo = synthesize_lens
        lens_handler
        rc
        lc
        (name,r1,r2,exs)
        iteratively_deepen_strategy
    in
    let lc = begin match lo with
      | None -> lc
      | Some l -> LensContext.insert_exn lc name l r1 r2
    end in
    (rc,lc)
  end

let run_declarations (lens_handler:(dnf_lens*regex*regex*RegexContext.t) option -> regex ->
  regex -> examples -> RegexContext.t -> LensContext.t -> unit) (test_handler:string -> unit)(ds:declaration list) (iteratively_deepen_strategy:bool) : unit =
  let _ = (List.fold_left
    ~f:(fun (rc,lc) d -> run_declaration lens_handler test_handler rc lc d iteratively_deepen_strategy)
    ~init:(RegexContext.empty,LensContext.empty)
    ds) in
  ()
