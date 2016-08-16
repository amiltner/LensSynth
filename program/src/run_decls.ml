open Core.Std
open Regexcontext
open Lenscontext
open Eval
open Gen
open Lang

type regex_handler =
  string ->
  regex ->
  unit

type test_handler =
  string ->
  unit

type lens_handler =
  lens option ->
  string ->
  regex ->
  regex ->
  examples ->
  RegexContext.t ->
  LensContext.t ->
  unit

let create_userdef
    (rh:regex_handler)
    (c:RegexContext.t)
    (userdef_name:string)
    (userdef_meaning:regex)
    (concrete:bool)
  : (RegexContext.t) =
  let rec check_welldefined_regex (r:regex) : unit =
    begin match r with
    | RegExEmpty -> ()
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
  rh userdef_name userdef_meaning;
  (RegexContext.insert_exn c userdef_name userdef_meaning (not concrete))

let create_userdef_lens
    (lc:LensContext.t)
    (lens_name:id)
    (stype:regex)
    (ttype:regex)
    (lens_implementation:lens)
  : LensContext.t =
  LensContext.insert_exn lc lens_name lens_implementation stype ttype

let test_string (showing_strategy:string -> unit)
                (c:RegexContext.t)
                (r:regex)
                (s:string)
                : unit =
  let evaluation_result_string =
    (if fast_eval c r s then
      (s ^ " matches regular expression " ^ (string_of_regex r))
    else
      (s ^ " does not match regular expression " ^ (string_of_regex r)))
  in
  showing_strategy evaluation_result_string

let synthesize_lens
    (lh:lens_handler)
    (rc:RegexContext.t)
    (lc:LensContext.t)
    ((n,r1,r2,exs):specification)
  : lens option =
  let lo = gen_lens rc lc r1 r2 exs in
  lh lo n r1 r2 exs rc lc;
  lo

let run_declaration
    (lh:lens_handler)
    (rh:regex_handler)
    (th:test_handler)
    (rc:RegexContext.t)
    (lc:LensContext.t)
    (d:declaration)
  : (RegexContext.t * LensContext.t) =
  begin match d with
    | DeclRegexCreation (s,r,b) -> (create_userdef rh rc s r b, lc)
    | DeclTestString (r,s) -> (test_string th rc r s); (rc,lc)
    | DeclSynthesizeLens (name,r1,r2,exs) ->
      let lo = synthesize_lens
          lh
          rc
          lc
          (name,r1,r2,exs)
      in
      let lc = begin match lo with
        | None -> lc
        | Some l -> LensContext.insert_exn lc name l r1 r2
      end in
      (rc,lc)
    | DeclLensCreation (s,r1,r2,l) ->
      let lc = create_userdef_lens
        lc
        s
        r1
        r2
        l
      in
      (rc,lc)
    | DeclTestLens _ -> (*TODO*)(rc,lc)
  end
  
let run_declarations
    (lh:lens_handler)
    (th:test_handler)
    (rh:regex_handler)
    (ds:declaration list)
  : unit =
  let _ = (List.fold_left
    ~f:(fun (rc,lc) d -> run_declaration lh rh th rc lc d)
    ~init:(RegexContext.empty,LensContext.empty)
    ds) in
  ()
