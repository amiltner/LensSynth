open Core.Std
open Printf
open Util
open Fn

(**** Language {{{ *****)

exception Internal_error of string
let internal_error f s = raise @@ Internal_error (sprintf "(%s) %s" f s)

type regex =
  | RegExEmpty
  | RegExBase of string
  | RegExConcat of regex * regex
  | RegExOr of regex * regex
  | RegExStar of regex
  | RegExUserDefined of string
  | RegExMappedUserDefined of int

type examples = (string * string) list

type specification = (string * regex * regex * (string * string) list)

type context = (string * regex) list

type evaluation_context = context

type declaration =
  | DeclUserdefCreation of (string * regex * bool)
  | DeclTestString of (regex * string)
  | DeclSynthesizeProgram of specification

type program = declaration list

type synth_problems = (string * regex * bool) list * (specification list) 

type synth_problem = (context * evaluation_context * string * regex * regex * (string * string) list)

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



type atom =
  | AMappedUserDefined of int
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
  | RegExMappedUserDefined t -> atom_to_dnf_regex (AMappedUserDefined t)
  | RegExEmpty -> []
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
  | (AMappedUserDefined s1, AMappedUserDefined s2) ->
      int_to_comparison (compare s1 s2)
  | (AMappedUserDefined _, _) -> LT
  | (_, AMappedUserDefined _) -> GT
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
  | EAMappedUserDefined of int * string list * int list list
  | EAUserDefined of string * string list * int list list
  | EAStar of exampled_dnf_regex * int list list

and exampled_clause = (exampled_atom) list * string list * (int list list)

and exampled_dnf_regex = exampled_clause list * int list list

type exampled_regex =
  | ERegExEmpty
  | ERegExBase of string * (int list list)
  | ERegExConcat of exampled_regex * exampled_regex * (int list list)
  | ERegExOr of exampled_regex  * exampled_regex * (int list list)
  | ERegExStar of exampled_regex * (int list list)
  | ERegExUserDefined of string * string list * (int list list)
  | ERegExMappedUserDefined of int * string list * (int list list)

let rec extract_example_list (er:exampled_regex) : int list list =
  begin match er with
  | ERegExEmpty -> []
  | ERegExBase (_,il) -> il
  | ERegExConcat (_,_,il) -> il
  | ERegExOr (_,_,il) -> il
  | ERegExStar (_,il) -> il
  | ERegExUserDefined (_,_,il) -> il
  | ERegExMappedUserDefined (_,_,il) -> il
  end

type ordered_exampled_atom =
  | OEAMappedUserDefined of int
  | OEAUserDefined of string * string list
  | OEAStar of ordered_exampled_dnf_regex

and ordered_exampled_clause = ((ordered_exampled_atom * int) list) list * string
list * (int list list)

and ordered_exampled_dnf_regex = (ordered_exampled_clause * int) list list

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
    | (OEAMappedUserDefined s, OEAMappedUserDefined t) ->
        comparison_compare s t
    | (OEAMappedUserDefined _, _) -> LT
    | (_, OEAMappedUserDefined _) -> GT
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
  | EAMappedUserDefined (s,_,_) -> OEAMappedUserDefined s
  | EAUserDefined (s,el,_) -> OEAUserDefined (s,el)
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

let rec or_size (r:regex) : int =
  begin match r with
  | RegExMappedUserDefined _ -> 1
  | RegExEmpty -> 0
  | RegExBase _ -> 1
  | RegExConcat (r1,r2) -> (or_size r1) * (or_size r2)
  | RegExOr (r1,r2) -> (or_size r1) + (or_size r2)
  | RegExStar r' -> or_size r'
  | RegExUserDefined _ -> 1
  end

let rec true_max_size (c:context) (r:regex) : int =
  begin match r with
  | RegExMappedUserDefined _ -> 1
  | RegExEmpty -> 0
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

let rec merge_contexts (c1:context) (c2:context) : context =
  c1@c2

let rec ordered_exampled_atom_to_atom
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

let rec atom_to_regex (a:atom) : regex =
  begin match a with
  | AMappedUserDefined t -> RegExMappedUserDefined t
  | AUserDefined t -> RegExUserDefined t
  | AStar dr -> RegExStar (dnf_regex_to_regex dr)
  end

and clause_to_regex ((atoms,strings):clause) : regex =
  let atoms_regex_list = List.map
    ~f:(fun a -> atom_to_regex a)
    atoms
  in
  let (hstr,tstr) = split_by_first_exn strings in
  let aslist = List.zip_exn atoms_regex_list tstr in
  List.fold_left
    ~f:(fun acc (a,s) ->
      RegExConcat(acc,RegExConcat(a,RegExBase s)))
    ~init:(RegExBase hstr)
    aslist

and dnf_regex_to_regex (r:dnf_regex) : regex =
  let sequence_regex_list = List.map
    ~f:(fun c -> clause_to_regex c)
    r
  in
  List.fold_left
  ~f:(fun acc sqr ->
    RegExOr (acc,sqr))
  ~init:RegExEmpty
  sequence_regex_list


      

type queue_element =
  | QERegexCombo of regex * regex * int * mapsbetweencontext
  | QEGenerator of (unit -> ((queue_element * float) list))


let rec clause_to_kvp ((atoms,strings):clause)
      : ((atom * string) option * clause) =
  begin match (atoms,strings) with
  | (ah::at,sh::st) -> (Some (ah,sh), (at,st))
  | ([],[s]) -> (None,([],[s]))
  | _ -> failwith "malformed clause"
  end

let rec tl_regex_to_regex (tl:((atom * string) option,
string) tagged_list_tree) : regex =
  begin match tl with
  | Leaf s -> RegExBase s
  | Node (aso,tll) ->
      begin match aso with
      | None -> tll_regex_to_regex tll
      | Some(a,s) ->
          let left_side = RegExConcat (RegExBase s,smart_atom_to_regex a) in
          let right_side = tll_regex_to_regex tll in
          RegExConcat(left_side,right_side)
      end
  end
and tll_regex_to_regex (tll:((atom * string) option,
string) tagged_list_tree list) : regex =
        List.fold_left
          ~f:(fun acc l -> RegExOr(acc, tl_regex_to_regex l))
          ~init:RegExEmpty
          tll

and smart_dnf_regex_to_regex (r:dnf_regex) : regex =
  let tltl = dnf_regex_to_tagged_list_tree_list r in
  let tltl_grouped = List.map ~f:tagged_list_tree_keygrouped tltl in
  let real_grouped_tltl = handle_noded_tltl tltl_grouped in
  tll_regex_to_regex tltl

and dnf_regex_to_tagged_list_tree_list (r:dnf_regex) : ((atom * string) option,
string) tagged_list_tree list =
  let kvp_list = List.map ~f:clause_to_kvp r in
  let keys = List.dedup (List.map ~f:fst kvp_list) in
  let test = List.fold_left
    ~f:(fun acc (k,v) ->
        insert_into_correct_list (k,v) acc)
    ~init:(List.map ~f:(fun key -> (key,[])) keys)
    kvp_list
  in
  List.map
    ~f:(fun (k,vl) ->
      begin match k with
      | None -> Node (k,
          List.map
            ~f:(fun x ->
              begin match x with
              | ([],[s]) -> Leaf s
              | _ -> failwith "bad"
              end)
            vl)
      | Some _ -> Node (k, dnf_regex_to_tagged_list_tree_list vl)
      end)
    test

and smart_atom_to_regex (a:atom) : regex =
  begin match a with
  | AMappedUserDefined t -> RegExMappedUserDefined t
  | AUserDefined t -> RegExUserDefined t
  | AStar dr -> RegExStar (smart_dnf_regex_to_regex dr)
  end



(***** }}} *****)
