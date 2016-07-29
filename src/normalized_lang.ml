open Lens
open Lang
open Lenscontext
open Core.Std
open Util


type exampled_atom =
  | EAMappedPart of int * string list * int list list
  | EAVariable of string * string * lens * string list * int list list
  | EAStar of exampled_dnf_regex * int list list

and exampled_clause = (exampled_atom) list * string list * (int list list)

and exampled_dnf_regex = exampled_clause list * int list list


type ordered_exampled_atom =
  | OEAMappedUserDefined of int
  | OEAUserDefined of string * string * lens * string list
  | OEAStar of ordered_exampled_dnf_regex

and ordered_exampled_clause = ((ordered_exampled_atom * int) list) list * string
list * (int list list)

and ordered_exampled_dnf_regex = (ordered_exampled_clause * int) list list

let rec compare_exampled_atoms (a1:exampled_atom) (a2:exampled_atom) :
  comparison =
    begin match (a1,a2) with
    | (EAVariable (s1,_,_,el1,_), EAVariable (s2,_,_,el2,_)) ->
        begin match (int_to_comparison (compare s1 s2)) with
        | EQ -> ordered_partition_order
                  (fun x y -> int_to_comparison (compare x y))
                  el1
                  el2
        | x -> x
        end
    | (EAStar (r1,_), EAStar (r2,_)) -> compare_exampled_dnf_regexs r1 r2
    | _ -> EQ
    end 

and compare_exampled_clauses ((atoms1,strings1,ints1):exampled_clause)
                             ((atoms2,strings2,ints2):exampled_clause)
                        : comparison =
  begin match ordered_partition_order compare_exampled_atoms atoms1 atoms2 with
  | EQ -> ordered_partition_order
            (fun x y -> int_to_comparison (compare x y))
            ints1
            ints2
  | c -> c
  end

and compare_exampled_dnf_regexs ((r1,_):exampled_dnf_regex) ((r2,_):exampled_dnf_regex) : comparison =
  ordered_partition_order
    compare_exampled_clauses
      r1
      r2

let rec compare_ordered_exampled_atoms (a1:ordered_exampled_atom)
                                       (a2:ordered_exampled_atom)
                                     : comparison =
    begin match (a1,a2) with
    | (OEAMappedUserDefined s, OEAMappedUserDefined t) ->
        comparison_compare s t
    | (OEAMappedUserDefined _, _) -> LT
    | (_, OEAMappedUserDefined _) -> GT
    | (OEAUserDefined (s1,s1',_,el1), OEAUserDefined (s2,s2',_,el2)) ->
        begin match (int_to_comparison (compare s1 s2)) with
        | EQ -> dictionary_order
                  (int_comparer_to_comparer compare)
                  el1
                  el2
        | x -> x
        end
    | (OEAStar r1, OEAStar r2) -> compare_ordered_exampled_dnf_regexs r1 r2
    | (OEAStar _, OEAUserDefined _) -> GT
    | (OEAUserDefined _, OEAStar _) -> LT
    end 

and compare_ordered_exampled_clauses
        ((atoms_partitions1,strings1,ints1):ordered_exampled_clause)
        ((atoms_partitions2,strings2,ints2):ordered_exampled_clause)
      : comparison =
  begin match ordered_partition_dictionary_order
                compare_ordered_exampled_atoms
                atoms_partitions1
                atoms_partitions2 with
  | EQ -> dictionary_order
            (fun x y -> int_to_comparison (compare x y))
            ints1
            ints2
  | c -> c
  end

and compare_ordered_exampled_dnf_regexs (r1:ordered_exampled_dnf_regex)
  (r2:ordered_exampled_dnf_regex) : comparison =
    ordered_partition_dictionary_order
      compare_ordered_exampled_clauses
        r1
        r2

let rec to_ordered_exampled_atom (a:exampled_atom) : ordered_exampled_atom =
  begin match a with
  | EAMappedPart (s,_,_) -> OEAMappedUserDefined s
  | EAVariable (s,sorig,lmap,el,_) -> OEAUserDefined (s,sorig,lmap,el)
  | EAStar (r,_) -> OEAStar (to_ordered_exampled_dnf_regex r)
  end

and to_ordered_exampled_clause ((atoms,strings,exnums):exampled_clause) : ordered_exampled_clause =
  let ordered_atoms = List.map ~f:to_ordered_exampled_atom atoms in
  let ordered_ordered_atoms =
    sort_and_partition_with_indices
      compare_ordered_exampled_atoms
      ordered_atoms in
  (ordered_ordered_atoms,strings,(List.sort ~cmp:compare exnums))

and to_ordered_exampled_dnf_regex ((r,_):exampled_dnf_regex)
        : ordered_exampled_dnf_regex =
  let ordered_clauses = List.map ~f:to_ordered_exampled_clause r in
  sort_and_partition_with_indices
    compare_ordered_exampled_clauses
    ordered_clauses

(*let rec ordered_exampled_atom_to_atom
  (a:ordered_exampled_atom)
  : atom =
    begin match a with
    | OEAMappedUserDefined t -> AMappedUserDefined t
    | OEAStar r -> AStar (ordered_exampled_dnf_regex_to_dnf_regex r)
    | OEAUserDefined (t,_) -> AUserDefined t
    end

and ordered_exampled_clause_to_clause
  ((c,ss,_):ordered_exampled_clause)
  : clause =
    let flattened_data = List.concat c in
    let unordered_atoms =
      List.sort ~cmp:(fun (_,i) (_,j) -> compare i j)
      flattened_data
    in
    (List.map
      ~f:(fun (a,_) -> ordered_exampled_atom_to_atom a)
      unordered_atoms
    ,ss)

and ordered_exampled_dnf_regex_to_dnf_regex
  (r:ordered_exampled_dnf_regex)
  : dnf_regex =
    let flattened_data = List.concat r in
    let unordered_clauses =
      List.sort ~cmp:(fun (_,i) (_,j) -> compare i j)
      flattened_data
    in
    List.map
      ~f:(fun (c,_) -> ordered_exampled_clause_to_clause c)
      unordered_clauses
*)
