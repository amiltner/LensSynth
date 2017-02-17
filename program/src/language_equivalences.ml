open Core.Std
open Lang
open Util
open Normalized_lang


let rec to_dnf_regex (r:regex) : dnf_regex =
  let atom_to_dnf_regex (a:atom) : dnf_regex =
    [([a],["";""])]
  in
  begin match r with
  | RegExEmpty -> []
  | RegExBase c -> [([],[c])]
  | RegExConcat (r1,r2) ->
      cartesian_map
        (fun (a1s,s1s) (a2s,s2s) -> (a1s@a2s,weld_lists (^) s1s s2s))
        (to_dnf_regex r1)
        (to_dnf_regex r2)
  | RegExOr (r1, r2) -> (to_dnf_regex r1) @ (to_dnf_regex r2)
  | RegExStar (r') -> atom_to_dnf_regex (AStar (to_dnf_regex r'))
  | RegExVariable s -> atom_to_dnf_regex (AUserDefined s)
  end


let rec atom_to_regex (a:atom) : regex =
  begin match a with
  | AUserDefined t -> RegExVariable t
  | AStar dr -> RegExStar (dnf_regex_to_regex dr)
  end

and clause_to_regex ((atoms,strings):clause) : regex =
  let atoms_regex_list = List.map
    ~f:(fun a -> atom_to_regex a)
    atoms
  in
  let (hstr,tstr) = split_by_first_exn strings in
  let aslist = List.zip_exn atoms_regex_list tstr in
  List.fold_right
    ~f:(fun (a,s) acc ->
      RegExConcat(RegExConcat(a,RegExBase s),acc))
    ~init:(RegExBase hstr)
    aslist

and dnf_regex_to_regex (r:dnf_regex) : regex =
  let sequence_regex_list = List.map
    ~f:(fun c -> clause_to_regex c)
    r
  in
  List.fold_right
  ~f:(fun sqr acc ->
    RegExOr (sqr,acc))
  ~init:RegExEmpty
  sequence_regex_list



type queue_element =
  | QERegexCombo of regex * regex * float * int
  | QEGenerator of (unit -> ((queue_element * float) list))


let clause_to_kvp ((atoms,strings):clause)
      : ((atom * string) option * clause) =
  begin match (atoms,strings) with
  | (ah::at,sh::st) -> (Some (ah,sh), (at,st))
  | ([],[s]) -> (None,([],[s]))
  | _ -> failwith "malformed clause"
  end

let rec tl_regex_to_regex (tl:(((atom * string) option) list,
string) tagged_list_tree) : regex =
  begin match tl with
  | Leaf s -> RegExBase s
  | Node (asl,tll) ->
      let left_side = List.fold_left
        ~f:(fun acc aso ->
          begin match aso with
          | None -> RegExBase ""
          | Some (a,s) ->
              RegExOr(acc,RegExConcat (RegExBase s,smart_atom_to_regex a))
          end)
        ~init:RegExEmpty
        asl
      in
      let right_side = tll_regex_to_regex tll in
      RegExConcat(left_side,right_side)
  end

and tll_regex_to_regex (tll:(((atom * string) option) list,
string) tagged_list_tree list) : regex =
        List.fold_left
          ~f:(fun acc l -> RegExOr(acc, tl_regex_to_regex l))
          ~init:RegExEmpty
          tll

and smart_dnf_regex_to_regex (r:dnf_regex) : regex =
  let tltl = dnf_regex_to_tagged_list_tree_list r in
  let tltl_grouped = List.map ~f:tagged_list_tree_keygrouped tltl in
  let real_grouped_tltl = handle_noded_tltl tltl_grouped in
  tll_regex_to_regex real_grouped_tltl

and dnf_regex_to_tagged_list_tree_list (r:dnf_regex) : ((atom * string) option,
string) tagged_list_tree list =
  let kvp_list = List.map ~f:clause_to_kvp r in
  let keys = List.dedup (List.map ~f:fst kvp_list) in
  let test = List.fold_left
    ~f:(fun acc (k,v) ->
        insert_into_correct_list acc k v)
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
  | AUserDefined t -> RegExVariable t
  | AStar dr -> RegExStar (smart_dnf_regex_to_regex dr)
  end
