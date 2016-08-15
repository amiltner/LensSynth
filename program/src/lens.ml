open Lang
open String_utilities
open Regex

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

