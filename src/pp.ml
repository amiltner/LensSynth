open Lang
open Lens
open Core.Std
open Permutation
open Util

let paren (s:string) : string = "(" ^ s ^ ")"

let bracket (s:string) : string = "[" ^ s ^ "]"

let pp_int_list (il:int list) : string =
  bracket (
    String.concat
      ~sep:";"
      (List.map ~f:string_of_int il)
  )

let pp_int_list_list (ill:int list list) : string =
  bracket (
    String.concat
      ~sep:";"
      (List.map ~f:pp_int_list ill)
  )

let rec pp_regexp (r:regex) : string =
  begin match r with
  | RegExBase s -> s
  | RegExConcat (r1,r2) -> paren ((pp_regexp r1) ^ (pp_regexp r2))
  | RegExOr (r1,r2) -> paren ((pp_regexp r1) ^ "|" ^ (pp_regexp r2))
  | RegExStar (r') -> paren (pp_regexp r') ^ "*"
  | RegExUserDefined s -> s
  end

let rec pp_lens (l:lens) : string =
  begin match l with
  | ConstLens (s1,s2) -> "const('" ^ s1 ^ "','" ^ s2 ^ "')"
  | ConcatLens (l1,l2) -> paren (pp_lens l1) ^ "." ^ (paren (pp_lens l2))
  | ComposeLens (l1,l2) -> paren (pp_lens l1) ^ ";" ^ (paren (pp_lens l2))
  | SwapLens (l1,l2) -> "swap(" ^ (pp_lens l1) ^ "," ^ (pp_lens l2) ^ ")"
  | UnionLens (l1,l2) -> paren (pp_lens l1) ^ "|" ^ (paren (pp_lens l2))
  | IterateLens (l') -> paren (pp_lens l') ^ "*"
  | IdentityLens -> "id"
  | RetypeLens (l,r1,r2) -> "retype(" ^ pp_lens l ^ "," ^ pp_regexp r1 ^ "," ^
  pp_regexp r2 ^ ")"
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

let rec pp_exampled_dnf_regex ((r,ill):exampled_dnf_regex) : string =
  paren ((String.concat
  ~sep:" + "
  (List.map ~f:pp_exampled_clause r)) ^ "," ^ (pp_int_list_list ill))

and pp_exampled_clause ((atoms,strings,examples):exampled_clause) : string =
  paren (bracket (
    String.concat
    ~sep:";"
    (List.map ~f:pp_exampled_atom atoms)))
  ^ "," ^
  (bracket (
    String.concat
    ~sep:";"
    strings))
  ^ "," ^
  (bracket (
    String.concat
    ~sep:";"
        (List.map ~f:
          (fun il -> bracket (String.concat ~sep:";"
          (List.map ~f:string_of_int il)))
          examples)))

and pp_exampled_atom (a:exampled_atom) : string =
  begin match a with
  | EAUserDefined (s,sl,ill) -> paren (
      s ^ "," ^
      bracket (
        String.concat
        ~sep:";"
        sl) ^ "," ^ pp_int_list_list ill
      )
  | EAStar (r,ill) -> (paren ((pp_exampled_dnf_regex r) ^ (pp_int_list_list ill))) ^ "*"
  end

(*let rec pp_exampled_ordered_dnf_regex ((r):ordered_exampled_dnf_regex) : string =
  paren ((String.concat
  ~sep:" + "
  (List.map (fun cs -> (String.concat ~sep:"\n"
    (List.map ~f:(fun (x,i) -> pp_ordered_exampled_clause x) cs))) r)))

and pp_ordered_exampled_clause ((atomintl,strings,ill):ordered_exampled_clause) : string =
  paren (bracket (
    String.concat
    ~sep:";"
    (List.map ~f:pp_exampled_atom atomintl)))
  ^ "," ^
  (bracket (
    String.concat
    ~sep:";"
    strings))
  ^ "," ^
  (bracket (
    String.concat
    ~sep:";"
        (List.map ~f:
          (fun il -> bracket (String.concat ~sep:";"
          (List.map ~f:string_of_int il)))
          ill)))

and pp_ordered_exampled_atom (a:exampled_atom) : string =
  begin match a with
  | EAUserDefined (s,sl,ill) -> paren (
      s ^ "," ^
      bracket (
        String.concat
        ~sep:";"
        sl) ^ "," ^ pp_int_list_list ill
      )
  | EAStar (r,ill) -> (paren ((pp_exampled_dnf_regex r) ^ (pp_int_list_list ill))) ^ "*"
  end*)



let rec pp_exampled_regex (r:exampled_regex) : string =
  begin match r with
  | ERegExBase (s,ill) -> paren (s ^ pp_int_list_list ill)
  | ERegExConcat (r1,r2,ill) -> paren ((pp_exampled_regex r1) ^ (pp_exampled_regex
  r2) ^ pp_int_list_list ill)
  | ERegExOr (r1,r2,ill) ->
      paren(
      paren (pp_exampled_regex r1)
      ^ "+"
      ^ paren (pp_exampled_regex r2)
      ^ pp_int_list_list ill)
  | ERegExStar (r',ill) ->
      paren (paren (pp_exampled_regex r') ^ "*" ^ pp_int_list_list ill)
  | ERegExUserDefined (s,ss,ill) -> paren (s ^ (bracket (
      String.concat
      ~sep:";"
      ss) ^ pp_int_list_list ill))
  end

let rec pp_dnf_regex_as_dnf_regex (clauses:dnf_regex) : string =
  (String.concat (List.map ~f:(fun c -> paren (pp_clause c)) clauses) ~sep:"+")

and pp_atom (a:atom) : string =
  begin match a with
  | AStar dnf_rx -> (paren (pp_dnf_regex_as_dnf_regex dnf_rx)) ^ "*"
  | AUserDefined s -> s
  end

and pp_clause ((atoms,strings):clause) : string =
  paren ((paren (String.concat (List.map ~f:pp_atom atoms) ~sep:","))
    ^ ","
    ^ (paren (String.concat strings ~sep:",")))

let rec pp_dnf_regex_as_regex (clauses:dnf_regex) : string =
  let pp_atom (a:atom) : string =
    begin match a with
    | AStar dnf_rx -> (paren (pp_dnf_regex_as_regex dnf_rx)) ^ "*"
    | AUserDefined s -> paren s
    end in
    let pp_clause ((atoms,strings):clause) : string =
      begin match strings with
      | h::t -> let zipped_tail = List.zip_exn atoms t in
          h ^ String.concat (List.map ~f:(fun (a,s) -> (pp_atom a) ^ s) zipped_tail)

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
        String.concat (List.map ~f:(fun x -> "'"^x^"'") strings1) ~sep:","

      )
      ^ " , " ^
      paren (
        String.concat (List.map ~f:(fun x -> "'"^x^"'") strings2) ~sep:","
      )
    ) in
  paren (
    String.concat (List.map ~f:pp_clause_lens clause_lenses) ~sep:","
  )
  ^ " , " ^
  paren (
    Permutation.pp permutation
  )

let pp_comparison (c:comparison) : string =
  begin match c with
  | EQ -> "EQ"
  | LT -> "LT"
  | GT -> "GT"
  end
