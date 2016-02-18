open Lang
open Core.Std

let rec pp_regexp (r:regex) : string =
  begin match r with
  | RegExBase c -> Char.to_string c
  | RegExConcat (r1,r2) -> (pp_regexp r1) ^ (pp_regexp r2)
  | RegExOr (r1,r2) -> "(" ^ (pp_regexp r1) ^ "|" ^ (pp_regexp r2) ^ ")"
  | RegExStar (r') -> "(" ^ (pp_regexp r') ^ ")*"
  end

let rec pp_normalized_regex (r:normalized_regex) : string =
  let rec pp_basis_regex (r:basis_subex) : string =
    begin match r with
    | NRXBase c -> Char.to_string c
    | NRXStar r' -> "(" ^ (pp_normalized_regex r') ^ ")*"
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
