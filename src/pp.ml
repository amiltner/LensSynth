open Lang
open Lens
open Core.Std
open Permutation

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
    | NRXBase s -> s
    | NRXStar r' -> "(" ^ (pp_normalized_regex r') ^ ")*"
    | NRXUserDefined s -> s
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

let rec pp_dnf_regex_as_dnf_regex (clauses:dnf_regex) : string =
  let pp_atom (a:atom) : string =
    begin match a with
    | AStar dnf_rx -> (paren (pp_dnf_regex_as_dnf_regex dnf_rx)) ^ "*"
    | AUserDefined s -> s
    end in
    let pp_clause ((atoms,strings):clause) : string =
      paren ((paren (String.concat (List.map ~f:pp_atom atoms) ~sep:","))
        ^ ","
        ^ (paren (String.concat strings ~sep:","))) in
    (String.concat (List.map ~f:(fun c -> paren (pp_clause c)) clauses) ~sep:"+")

let rec pp_dnf_regex_as_regex (clauses:dnf_regex) : string =
  let pp_atom (a:atom) : string =
    begin match a with
    | AStar dnf_rx -> (paren (pp_dnf_regex_as_regex dnf_rx)) ^ "*"
    | AUserDefined s -> paren s
    end in
    let pp_clause ((atoms,strings):clause) : string =
      begin match strings with
      | h::t -> let zipped_tail = List.zip_exn atoms t in
          h ^ String.concat (List.map ~f:(fun (a,s) -> s ^ (pp_atom a)) zipped_tail)

      | [] -> failwith "bad clause"
      end in
    (String.concat (List.map ~f:(fun c -> pp_clause c) clauses) ~sep:"+")

let rec pp_dnf_lens ((clause_lenses, permutation):dnf_lens) : string =
  let pp_atom_lens (a:atom_lens) : string =
    begin match a with
    | AIterate l -> "iterate" ^ (paren (pp_dnf_lens l))
    | AIdentity -> "identity"
    end in
  let pp_clause_lens ((atomls,permutation,strings1,strings2):clause_lens) : string =
    paren (
      paren (
        String.concat (List.map ~f:pp_atom_lens atomls) ~sep:","
      )
      ^ " , " ^
      paren (
        Permutation.pp permutation
      )
      ^ " , " ^
      paren (
        String.concat strings1 ~sep:","
      )
      ^ " , " ^
      paren (
        String.concat strings2 ~sep:","
      )
    ) in
  paren (
    String.concat (List.map ~f:pp_clause_lens clause_lenses) ~sep:","
  )
  ^ " , " ^
  paren (
    Permutation.pp permutation
  )
