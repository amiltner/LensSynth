open Lang
open Lens
open Core.Std

let paren (s:string) : string = "(" ^ s ^ ")"

let rec pp_regexp (r:regex) : string =
  begin match r with
  | RegExBase s -> s
  | RegExConcat (r1,r2) -> (pp_regexp r1) ^ (pp_regexp r2)
  | RegExOr (r1,r2) -> paren ((pp_regexp r1) ^ "|" ^ (pp_regexp r2))
  | RegExStar (r') -> paren (pp_regexp r') ^ "*"
  | RegExUserDefined s -> s
  end

let rec pp_lens (l:lens) : string =
  begin match l with
  | ConstLens (s1,s2) -> "const(" ^ s1 ^ "," ^ s2 ^ ")"
  | ConcatLens (l1,l2) -> paren (pp_lens l1) ^ "." ^ (paren (pp_lens l2))
  | SwapLens (l1,l2) -> "swap(" ^ (pp_lens l1) ^ "," ^ (pp_lens l2) ^ ")"
  | UnionLens (l1,l2) -> paren (pp_lens l1) ^ "|" ^ (paren (pp_lens l2))
  | IterateLens (l') -> paren (pp_lens l') ^ "*"
  | IdentityLens -> "id"
  end

let rec pp_normalized_regex (r:normalized_regex) : string =
  let rec pp_basis_regex (r:basis_subex) : string =
    begin match r with
    | NRXStar r' -> "(" ^ (pp_normalized_regex r') ^ ")*"
    | NRXUserDefined s -> s
    | NRXBase c -> c
    end
  in
  let rec pp_concated_regex (r:concated_subex) : string =
    String.concat (List.map ~f:pp_basis_regex r) ~sep:""
    (*List.fold_left
      ~f:(fun acc x -> (pp_basis_regex x) ^ acc)
      ~init:""
      r*)
  in
  String.concat (List.map ~f:pp_concated_regex r) ~sep:"|"
