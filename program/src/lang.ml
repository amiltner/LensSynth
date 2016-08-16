open Core.Std
open Printf
open String_utilities




(**** General {{{ *****)

exception Internal_error of string
let internal_error f s = raise @@ Internal_error (sprintf "(%s) %s" f s)

type id = string

(***** }}} *****)





(**** Regex {{{ *****)

type regex =
  | RegExEmpty
  | RegExBase of string
  | RegExConcat of regex * regex
  | RegExOr of regex * regex
  | RegExStar of regex
  | RegExVariable of string

let multiplicative_identity_regex = RegExBase ""

let separate_plus_regex (r:regex) : (regex * regex) option =
  begin match r with
    | RegExOr (r1,r2) -> Some (r1,r2)
    | _ -> None
  end

let separate_times_regex (r:regex) : (regex * regex) option =
  begin match r with
    | RegExConcat (r1,r2) -> Some (r1,r2)
    | _ -> None
  end

let create_plus_regex (r1:regex) (r2:regex) : regex =
  RegExOr (r1,r2)

let create_times_regex (r1:regex) (r2:regex) : regex =
  RegExConcat (r1,r2)

let rec apply_at_every_level_regex (f:regex -> regex) (r:regex) : regex =
  let r =
    begin match r with
      | RegExConcat (r1,r2) ->
        RegExConcat (apply_at_every_level_regex f r1, apply_at_every_level_regex f r2)
      | RegExOr (r1,r2) ->
        RegExOr (apply_at_every_level_regex f r1, apply_at_every_level_regex f r2)
      | RegExStar r' ->
        RegExStar (apply_at_every_level_regex f r')
      | _ -> r
    end
  in
  f r

let rec string_of_regex (r:regex) : string =
  begin match r with
  | RegExEmpty -> "{}"
  | RegExBase s -> "\"" ^ s ^ "\""
  | RegExConcat (r1,r2) -> paren ((string_of_regex r1) ^ "" ^ (string_of_regex r2))
  | RegExOr (r1,r2) -> paren ((string_of_regex r1) ^ "|" ^ (string_of_regex r2))
  | RegExStar (r') -> paren (string_of_regex r') ^ "*"
  | RegExVariable s -> s
  end

(***** }}} *****)




(**** Lens {{{ *****)

type lens =
  | LensConst of string * string
  | LensConcat of lens * lens
  | LensSwap of lens * lens
  | LensUnion of lens * lens
  | LensCompose of lens * lens
  | LensIterate of lens
  | LensIdentity of regex
  | LensInverse of lens
  | LensVariable of id


let multiplicative_identity_lens = LensIdentity (multiplicative_identity_regex)

let separate_plus_lens (l:lens) : (lens * lens) option =
  begin match l with
    | LensUnion (l1,l2) -> Some (l1,l2)
    | _ -> None
  end

let separate_times_lens (l:lens) : (lens * lens) option =
  begin match l with
    | LensConcat (l1,l2) -> Some (l1,l2)
    | _ -> None
  end

let create_plus_lens (l1:lens) (l2:lens) : lens =
  LensUnion (l1,l2)

let create_times_lens (l1:lens) (l2:lens) : lens =
  LensConcat (l1,l2)

let rec string_of_lens (l:lens) : string =
  begin match l with
  | LensConst (s1,s2) -> "const('" ^ s1 ^ "','" ^ s2 ^ "')"
  | LensConcat (l1,l2) -> paren ((string_of_lens l1) ^ "." ^ (string_of_lens l2))
  | LensCompose (l1,l2) -> paren ((string_of_lens l1) ^ ";" ^ (string_of_lens l2))
  | LensSwap (l1,l2) -> "swap(" ^ (string_of_lens l1) ^ "," ^ (string_of_lens l2) ^ ")"
  | LensUnion (l1,l2) -> paren ((string_of_lens l1) ^ "|" ^ (string_of_lens l2))
  | LensIterate (l') -> paren (string_of_lens l') ^ "*"
  | LensIdentity r -> "id(" ^ (string_of_regex r) ^")"
  | LensInverse l' -> "inverse(" ^ (string_of_lens l') ^ ")"
  | LensVariable n -> n
  end

(***** }}} *****)



(**** Language {{{ *****)

type examples = (string * string) list

type specification = (string * regex * regex * (string * string) list)

type declaration =
  | DeclRegexCreation of (id * regex * bool)
  | DeclTestString of (regex * string)
  | DeclSynthesizeLens of specification
  | DeclLensCreation of id * regex * regex * lens
  | DeclTestLens of id * examples

type program = declaration list

type synth_problems = (string * regex * bool) list * (specification list) 

(***** }}} *****)


