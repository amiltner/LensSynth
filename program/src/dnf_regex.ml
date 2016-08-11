open Core.Std
open Util

type atom =
  | AMappedUserDefined of int
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
  | (AMappedUserDefined s1, AMappedUserDefined s2) ->
      int_to_comparison (compare s1 s2)
  | (AMappedUserDefined _, _) -> LT
  | (_, AMappedUserDefined _) -> GT
  | (AUserDefined s1, AUserDefined s2) -> int_to_comparison (compare s1 s2)
  | (AUserDefined  _, AStar         _) -> LT
  | (AStar         _, AUserDefined  _) -> GT
  | (AStar        r1, AStar        r2) -> compare_dnf_regexs r1 r2
  end

and compare_clauses ((atoms1,_):clause) ((atoms2,_):clause) : comparison =
  ordered_partition_order compare_atoms atoms1 atoms2

and compare_dnf_regexs (r1:dnf_regex) (r2:dnf_regex) : comparison =
  ordered_partition_order compare_clauses r1 r2




(* mbc *)
type mapsbetweencontext = ((int * (dnf_regex * dnf_regex)) list * int)

let emptymapsbetweencontext = ([],0)

let add_to_context  ((cl,n):mapsbetweencontext)
                    (r1:dnf_regex)
                    (r2:dnf_regex)
                    : mapsbetweencontext * int =
  begin match assoc_value_mem (r1,r2) cl with
  | Some k -> ((cl,n),k)
  | None -> (((n,(r1,r2))::cl,n+1),n)
  end

type mapsbetweencontextside = (int * dnf_regex) list

let get_left_side ((c,_):mapsbetweencontext) =
  List.map ~f:(fun (i,(l,_)) -> (i,l)) c

let get_right_side ((c,_):mapsbetweencontext) =
  List.map ~f:(fun (i,(l,_)) -> (i,l)) c
