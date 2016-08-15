open Lens
open Core.Std
open Util
open Permutation


type exampled_atom =
  | EAVariable of string * string * lens * string list * int list list
  | EAStar of exampled_dnf_regex * int list list

and exampled_clause = (exampled_atom) list * string list * (int list list)

and exampled_dnf_regex = exampled_clause list * int list list


type ordered_exampled_atom =
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

and compare_exampled_clauses ((atoms1,_,ints1):exampled_clause)
                             ((atoms2,_,ints2):exampled_clause)
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
    | (OEAUserDefined (s1,_,_,el1), OEAUserDefined (s2,_,_,el2)) ->
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
        ((atoms_partitions1,_,ints1):ordered_exampled_clause)
        ((atoms_partitions2,_,ints2):ordered_exampled_clause)
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



type atom_lens =
  | AtomLensIterate of dnf_lens
  | AtomLensVariable of lens

and clause_lens = atom_lens list * Permutation.t * string list * string list

and dnf_lens = clause_lens list * Permutation.t




type atom =
  | AUserDefined of string
  | AStar of dnf_regex

and clause = atom list * string list

and dnf_regex = clause list

let empty_dnf_string : dnf_regex = [([],[""])]

let concat_dnf_regexs (r1:dnf_regex) (r2:dnf_regex) : dnf_regex =
  cartesian_map
    (fun (a1s,s1s) (a2s,s2s) -> (a1s@a2s,weld_lists (^) s1s s2s))
    r1
    r2

let or_dnf_regexs (r1:dnf_regex) (r2:dnf_regex) : dnf_regex =
  r1 @ r2

let concat_clause_dnf_rx (cl:clause) (r:dnf_regex) : dnf_regex =
  concat_dnf_regexs [cl] r

let concat_dnf_rx_clause (r:dnf_regex) (cl:clause) : dnf_regex =
  concat_dnf_regexs r [cl]

let rec exponentiate_dnf (r:dnf_regex) (n:int) : dnf_regex =
  if n < 0 then
    failwith "invalid exponential"
  else if n = 0 then
    empty_dnf_string
  else
    concat_dnf_regexs (exponentiate_dnf r (n-1)) r

let rec quotiented_star_dnf (r:dnf_regex) (n:int) : dnf_regex =
  if n < 1 then
    failwith "invalid modulation"
  else if n = 1 then
    empty_dnf_string
  else
    or_dnf_regexs (quotiented_star_dnf r (n-1)) (exponentiate_dnf r (n-1))

let singleton_atom (a:atom) : dnf_regex =
  [([a],["";""])]


let rec compare_atoms (a1:atom) (a2:atom) : comparison =
  begin match (a1,a2) with
  | (AUserDefined s1, AUserDefined s2) -> int_to_comparison (compare s1 s2)
  | (AUserDefined  _, AStar         _) -> LT
  | (AStar         _, AUserDefined  _) -> GT
  | (AStar        r1, AStar        r2) -> compare_dnf_regexs r1 r2
  end

and compare_clauses ((atoms1,_):clause) ((atoms2,_):clause) : comparison =
  ordered_partition_order compare_atoms atoms1 atoms2

and compare_dnf_regexs (r1:dnf_regex) (r2:dnf_regex) : comparison =
  ordered_partition_order compare_clauses r1 r2




