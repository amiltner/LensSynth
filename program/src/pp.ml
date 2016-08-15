open String_utilities
open Lang
open Lens
open Core.Std
open Permutation
open Normalized_lang


let rec string_of_exampled_dnf_regex ((r,ill):exampled_dnf_regex) : string =
  paren ((String.concat
  ~sep:" + "
  (List.map ~f:string_of_exampled_clause r)) ^ "," ^ (string_of_int_list_list ill))

and string_of_exampled_clause ((atoms,strings,examples):exampled_clause) : string =
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
  | EAVariable (s,_,_,sl,ill) -> paren (
      s ^ "," ^
      bracket (
        String.concat
        ~sep:";"
        sl) ^ "," ^ string_of_int_list_list ill
      )
  | EAStar (r,ill) -> (paren ((string_of_exampled_dnf_regex r) ^ (string_of_int_list_list ill))) ^ "*"
  end

let rec pp_exampled_regex (r:exampled_regex) : string =
  begin match r with
  | ERegExBase (s,ill) -> paren (s ^ string_of_int_list_list ill)
  | ERegExConcat (r1,r2,ill) -> paren ((pp_exampled_regex r1) ^ (pp_exampled_regex
  r2) ^ string_of_int_list_list ill)
  | ERegExOr (r1,r2,ill) ->
    paren(
        paren (pp_exampled_regex r1)
      ^ "+"
      ^ paren (pp_exampled_regex r2)
      ^ string_of_int_list_list ill)
  | ERegExStar (r',ill) ->
    paren (paren (pp_exampled_regex r') ^ "*" ^ string_of_int_list_list ill)
  | ERegExVariable (s,ss,ill) -> paren (s ^ (bracket (
      String.concat
        ~sep:";"
        ss) ^ string_of_int_list_list ill))
  | ERegExEmpty -> "{}"
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
    | AtomLensIterate l -> "iterate" ^ (paren (pp_dnf_lens l))
    | AtomLensVariable lc -> "librarycall(" ^ (string_of_lens lc) ^ ")"
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
