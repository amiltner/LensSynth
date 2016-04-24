open Core.Std
open Counter
open Printf
open Util
open Fn

(**** Language {{{ *****)

exception Internal_error of string
let internal_error f s = raise @@ Internal_error (sprintf "(%s) %s" f s)

type regex =
  | RegExBase of string
  | RegExConcat of regex * regex
  | RegExOr of regex * regex
  | RegExStar of regex
  | RegExUserDefined of string

type examples = (string * string) list

type specification = (string * regex * regex * (string * string) list)

type context = (string * regex) list

type evaluation_context = context

type synth_problems = (string * regex * bool) list * (specification list) 

type synth_problem = (context * evaluation_context * string * regex * regex * (string * string) list)

type unioned_subex = concated_subex list

and concated_subex = basis_subex list

and basis_subex =
  | NRXBase of string
  | NRXStar of unioned_subex
  | NRXUserDefined of string

type normalized_regex = unioned_subex

type normalized_synth_problem = ((string * normalized_regex) list)
                                * normalized_regex * normalized_regex
                                * (string * string) list

let problems_to_problem_list ((ds,ss):synth_problems) : synth_problem list =
  let e_c = List.map ~f:(fun (t,d,_) -> (t,d)) ds in
  let c = List.filter_map
    ~f:(fun (t,d,s) ->
      if s then
        Some (t,d)
      else
        None)
    ds in
  List.map ~f:(fun (n,r1,r2,exl) -> (c,e_c,n,r1,r2,exl)) ss

let rec to_normalized_exp (r:regex) : normalized_regex =
  begin match r with
  | RegExBase c -> [[NRXBase c]]
  | RegExConcat (r1,r2) ->
      cartesian_map (@) (to_normalized_exp r1) (to_normalized_exp r2)
  | RegExOr (r1,r2) -> (to_normalized_exp r1) @ (to_normalized_exp r2)
  | RegExStar (r') -> [[NRXStar (to_normalized_exp r')]]
  | RegExUserDefined s -> [[NRXUserDefined s]]
  end

let rec to_normalized_synth_problem ((c,e_c,n,r1,r2,es):synth_problem)
: normalized_synth_problem =
  (List.map ~f:(fun (s,r) -> (s, to_normalized_exp r)) c, to_normalized_exp r1, to_normalized_exp r2, es)



type atom =
  | AUserDefined of string
  | AStar of dnf_regex

and clause = atom list * string list

and dnf_regex = clause list

let empty_dnf_string : dnf_regex = [([],[""])]

let rec concat_dnf_regexs (r1:dnf_regex) (r2:dnf_regex) : dnf_regex =
  cartesian_map
    (fun (a1s,s1s) (a2s,s2s) -> (a1s@a2s,weld_lists (^) s1s s2s))
    r1
    r2

let rec or_dnf_regexs (r1:dnf_regex) (r2:dnf_regex) : dnf_regex =
  r1 @ r2

let rec concat_clause_dnf_rx (cl:clause) (r:dnf_regex) : dnf_regex =
  concat_dnf_regexs [cl] r

let rec concat_dnf_rx_clause (r:dnf_regex) (cl:clause) : dnf_regex =
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

let rec singleton_atom (a:atom) : dnf_regex =
  [([a],["";""])]

let rec to_dnf_regex (r:regex) : dnf_regex =
  let rec atom_to_dnf_regex (a:atom) : dnf_regex =
    [([a],["";""])]
  in
  begin match r with
  | RegExBase c -> [([],[c])]
  | RegExConcat (r1,r2) ->
      cartesian_map
        (fun (a1s,s1s) (a2s,s2s) -> (a1s@a2s,weld_lists (^) s1s s2s))
        (to_dnf_regex r1)
        (to_dnf_regex r2)
  | RegExOr (r1, r2) -> (to_dnf_regex r1) @ (to_dnf_regex r2)
  | RegExStar (r') -> atom_to_dnf_regex (AStar (to_dnf_regex r'))
  | RegExUserDefined s -> atom_to_dnf_regex (AUserDefined s)
  end


let rec compare_atoms (a1:atom) (a2:atom) : comparison =
  begin match (a1,a2) with
  | (AUserDefined s1, AUserDefined s2) -> int_to_comparison (compare s1 s2)
  | (AUserDefined  _, AStar         _) -> LT
  | (AStar         _, AUserDefined  _) -> GT
  | (AStar        r1, AStar        r2) -> compare_dnf_regexs r1 r2
  end

and compare_clauses ((atoms1,strings1):clause) ((atoms2,strings2):clause) : comparison =
  ordered_partition_order compare_atoms atoms1 atoms2

and compare_dnf_regexs (r1:dnf_regex) (r2:dnf_regex) : comparison =
  ordered_partition_order compare_clauses r1 r2

type exampled_atom =
  | EAUserDefined of string * string list * int list list
  | EAStar of exampled_dnf_regex * int list list

and exampled_clause = (exampled_atom) list * string list * (int list list)

and exampled_dnf_regex = exampled_clause list * int list list

type exampled_regex =
  | ERegExBase of string * (int list list)
  | ERegExConcat of exampled_regex * exampled_regex * (int list list)
  | ERegExOr of exampled_regex  * exampled_regex * (int list list)
  | ERegExStar of exampled_regex * (int list list)
  | ERegExUserDefined of string * string list * (int list list)

let rec extract_example_list (er:exampled_regex) : int list list =
  begin match er with
  | ERegExBase (_,il) -> il
  | ERegExConcat (_,_,il) -> il
  | ERegExOr (_,_,il) -> il
  | ERegExStar (_,il) -> il
  | ERegExUserDefined (_,_,il) -> il
  end

type ordered_exampled_atom =
  | OEAUserDefined of string * string list
  | OEAStar of ordered_exampled_dnf_regex

and ordered_exampled_clause = ((ordered_exampled_atom * int) list) list * string
list * (int list list)

and ordered_exampled_dnf_regex = (ordered_exampled_clause * int) list list

let compare_int_list_list : int list list
                            -> int list list
                            -> comparison =
  ordered_partition_order
    comparison_compare

type ordered_exampled_compressed_atom =
  | OECAUserDefined of string * string list * int list list
  | OECAStar of ordered_exampled_compressed_dnf_regex * int list list

and ordered_exampled_compressed_clause =
  (ordered_exampled_compressed_atom Counter.t
  * int list list)

and ordered_exampled_compressed_dnf_regex =
  (ordered_exampled_compressed_clause Counter.t
  * int list list)

let rec clean_exampledness_atom (choices:int list list)
(a:ordered_exampled_compressed_atom)  : ordered_exampled_compressed_atom =
  begin match a with
  | OECAUserDefined (s,el,cs) ->
      let udef_choice_zip = List.zip_exn el cs in
      let actual_choices =
        List.filter
          ~f:(fun (el,c) -> List.mem choices c )
          udef_choice_zip
        in
        let (strs,cs) = List.unzip actual_choices in
      OECAUserDefined (s,strs,cs)
  | OECAStar (r,cs) ->
      
  let actual_choices =
    List.filter
      ~f:(fun ch -> List.mem choices ch)
      cs
        in
      
      OECAStar (clean_exampledness_dnf_regex actual_choices r, actual_choices)
  end
and clean_exampledness_clause (above_choices:int list list)
((atom_counter,current_choices):ordered_exampled_compressed_clause) :
  ordered_exampled_compressed_clause =


  let actual_choices =
    List.filter
      ~f:(fun ch -> List.mem above_choices ch)
      current_choices
        in

  (
    Counter.map (clean_exampledness_atom actual_choices) atom_counter,
    actual_choices)


and clean_exampledness_dnf_regex (above_choices:int list list)
((clause_counter,current_choices):ordered_exampled_compressed_dnf_regex)  :
  ordered_exampled_compressed_dnf_regex =

  let rec is_suplist (lowerc:int list) (upperc:int list) : bool =
    begin match (lowerc,upperc) with
    | (h1::t1,h2::t2) ->
        if h1 = h2 then
          is_suplist t1 t2
        else
          false
    | (_,[]) -> true
    | _ -> false
    end
  in
  let rec contains_sublist (viable_choices:int list list) (lowerc:int list) 
    : bool =
      List.exists ~f:(is_suplist (List.rev lowerc)) (List.map ~f:List.rev
      viable_choices)
  in

  let viable_choices = List.filter ~f:(contains_sublist above_choices)
  current_choices in



  (Counter.map (clean_exampledness_clause viable_choices) clause_counter,viable_choices)


let rec compare_ordered_exampled_compressed_atoms
        (a1:ordered_exampled_compressed_atom)
        (a2:ordered_exampled_compressed_atom)
        : comparison =
  begin match (a1,a2) with
  | (OECAUserDefined (s1,sl1,ill1), OECAUserDefined (s2,sl2,ill2)) ->
      begin match comparison_compare s1 s2 with
      | EQ -> begin match comparison_compare sl1 sl2 with
              | EQ ->
                  begin match ordered_partition_order
                                comparison_compare
                                sl1
                                sl2 with
                  | EQ -> compare_int_list_list ill1 ill2
                  | x -> x
                  end
              | x -> x
              end
       | x -> x
      end
  | (OECAStar (r1,ill1), OECAStar (r2,ill2)) ->
      begin match compare_ordered_exampled_compressed_dnf_regexs
                    r1
                    r2 with
      | EQ -> compare_int_list_list ill1 ill2
      | x -> x
      end
  | (OECAUserDefined _, OECAStar _) -> LT
  | (OECAStar _, OECAUserDefined _) -> GT
  end

and compare_ordered_exampled_compressed_clauses
      ((c1,ill1):ordered_exampled_compressed_clause)
      ((c2,ill2):ordered_exampled_compressed_clause)
      : comparison =
  begin match Counter.compare_counts
                c1
                c2 with
  | EQ -> compare_int_list_list ill1 ill2
  | x -> x
  end

and compare_ordered_exampled_compressed_dnf_regexs
      ((r1,ill1):ordered_exampled_compressed_dnf_regex)
      ((r2,ill2):ordered_exampled_compressed_dnf_regex)
      : comparison =
  begin match Counter.compare_counts
                r1
                r2 with
  | EQ -> compare_int_list_list ill1 ill2
  | x -> x
  end

let initial_atom_counter
  : ordered_exampled_compressed_atom Counter.t =
    Counter.create compare_ordered_exampled_compressed_atoms

let initial_clause_counter
  : ordered_exampled_compressed_clause Counter.t =
    Counter.create compare_ordered_exampled_compressed_clauses

let additive_identity (ill:int list list) =
  (initial_clause_counter, ill)

let multiplicative_identity_clause (ill:int list list) =
  (initial_atom_counter, ill)

let base_additive_identity = additive_identity []
let base_multiplicative_identity_clause =
  multiplicative_identity_clause []
let base_multiplicative_identity_atom =
  OECAStar (additive_identity [], [])

let rec ordered_exampled_compressed_atom_metric
        (a1:ordered_exampled_compressed_atom)
        (a2:ordered_exampled_compressed_atom)
        : int =
  begin match (a1,a2) with
  | (OECAUserDefined (s1,sl1,ill1), OECAUserDefined (s2,sl2,ill2)) ->
      discrete_metric comparison_compare (s1,sl1,ill1) (s2,sl2,ill2)
  | (OECAStar (r1,ill1), OECAStar (r2,ill2)) ->
      (ordered_exampled_compressed_dnf_regex_metric r1 r2)
      + (discrete_metric comparison_compare ill1 ill2)
  | (_, OECAStar (r,_)) ->
      1 + (ordered_exampled_compressed_dnf_regex_metric
        r
        (base_additive_identity))
  | (OECAStar (r,_), _) ->
      1 + (ordered_exampled_compressed_dnf_regex_metric
            r
            (base_additive_identity))
  end

and ordered_exampled_compressed_clause_metric
        ((c1,ill1):ordered_exampled_compressed_clause)
        ((c2,ill2):ordered_exampled_compressed_clause)
        : int =
  (counter_metric
    compare_ordered_exampled_compressed_atoms
    base_multiplicative_identity_atom
    ordered_exampled_compressed_atom_metric
    c1
    c2)
  + (discrete_metric comparison_compare ill1 ill2)

and ordered_exampled_compressed_dnf_regex_metric
        ((r1,ill1):ordered_exampled_compressed_dnf_regex)
        ((r2,ill2):ordered_exampled_compressed_dnf_regex)
        : int =
  (counter_metric
    compare_ordered_exampled_compressed_clauses
    base_multiplicative_identity_clause
    ordered_exampled_compressed_clause_metric
    r1
    r2)
  + (discrete_metric comparison_compare ill1 ill2)

let rec to_ordered_exampled_compressed_dnf_regex (r:exampled_regex)
  : ordered_exampled_compressed_dnf_regex =
    let create_singleton_atom
      (a:ordered_exampled_compressed_atom)
      (ill:int list list) :
        ordered_exampled_compressed_dnf_regex =
          (Counter.add
            initial_clause_counter
            (Counter.add
              initial_atom_counter a, ill), ill)
    in
    begin match r with
    | ERegExBase (s,ill) ->
        (Counter.add
          initial_clause_counter
          (multiplicative_identity_clause ill), ill)
    | ERegExUserDefined (t,sl,ill) ->
        create_singleton_atom (OECAUserDefined (t,sl,ill)) ill
    | ERegExStar (r',ill) ->
        let oecar' = to_ordered_exampled_compressed_dnf_regex r' in
        create_singleton_atom (OECAStar (oecar', ill)) ill
    | ERegExOr (r1,r2,ill) ->
        let (oecar1,ill1) =
          to_ordered_exampled_compressed_dnf_regex r1 in
        let (oecar2,ill2) =
          to_ordered_exampled_compressed_dnf_regex r2 in
        (Counter.merge oecar1 oecar2, ill)
    | ERegExConcat (r1,r2,ill) ->
        let (oecar1,ill1) =
          to_ordered_exampled_compressed_dnf_regex r1 in
        let (oecar2,ill2) =
          to_ordered_exampled_compressed_dnf_regex r2 in
        let newdnf = Counter.multiply_merge
          (fun (c1,ill1) (c2,ill2) ->
            let newill = intersect_ordered_no_dupes
              comparison_compare
              ill1 ill2 in
            (Counter.merge c1 c2,newill))
          oecar1
          oecar2
        in
        let newnewdnf = clean_exampledness_dnf_regex ill (newdnf,ill) in
        newnewdnf
    end



let rec compare_exampled_atoms (a1:exampled_atom) (a2:exampled_atom) :
  comparison =
    begin match (a1,a2) with
    | (EAUserDefined (s1,el1,_), EAUserDefined (s2,el2,_)) ->
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
    | (OEAUserDefined (s1,el1), OEAUserDefined (s2,el2)) ->
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
  | EAUserDefined (s,el,_) -> OEAUserDefined (s,el)
  | EAStar (r,_) -> OEAStar (to_ordered_exampled_dnf_regex r)
  end

and to_ordered_exampled_clause ((atoms,strings,exnums):exampled_clause) : ordered_exampled_clause =
  let ordered_atoms = List.map ~f:to_ordered_exampled_atom atoms in
  let ordered_ordered_atoms =
    sort_and_partition_with_indices
      compare_ordered_exampled_atoms
      ordered_atoms in
  (ordered_ordered_atoms,strings,exnums)

and to_ordered_exampled_dnf_regex ((r,_):exampled_dnf_regex)
        : ordered_exampled_dnf_regex =
  let ordered_clauses = List.map ~f:to_ordered_exampled_clause r in
  sort_and_partition_with_indices
    compare_ordered_exampled_clauses
    ordered_clauses

let rec or_size (r:regex) : int =
  begin match r with
  | RegExBase _ -> 1
  | RegExConcat (r1,r2) -> (or_size r1) * (or_size r2)
  | RegExOr (r1,r2) -> (or_size r1) + (or_size r2)
  | RegExStar r' -> or_size r'
  | RegExUserDefined _ -> 1
  end

let rec true_max_size (c:context) (r:regex) : int =
  begin match r with
  | RegExBase _ -> 1
  | RegExConcat (r1,r2) -> (true_max_size c r1) * (true_max_size c r2)
  | RegExOr (r1,r2) -> (true_max_size c r1) + (true_max_size c r2)
  | RegExStar r' -> true_max_size c r'
  | RegExUserDefined t ->
      begin match List.Assoc.find c t with
      | None -> 1
      | Some r' -> true_max_size c r'
      end
  end

(***** }}} *****)
