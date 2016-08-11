open Core.Std
open Lang
open Regex
open Lens
open Lens_utilities
open Datastructures
open Disjointset

(***** The main LensContext module {{{ *****)

module type LensContext_Sig = sig
    type t

    val empty                    : t
    val lookup_exn               : t -> id -> (lens * regex * regex)
    val lookup_type_exn          : t -> id -> regex * regex
    val lookup_impl_exn          : t -> id -> lens
    val insert_exn               : t -> id -> lens -> regex -> regex -> t
    val insert_list_exn          : t -> (id * lens * regex * regex) list -> t
    val create_from_list_exn     : (id * lens * regex * regex) list -> t
    val shortest_path_exn        : t -> id -> id -> lens
    val shortest_path_to_rep_elt : t -> id -> (id * lens)
end

(* add comments *)
(* declarative comments: google *)
(* SPECS not impl *)
(* here is the GOAL / meaning of what I'm doing *)
(* denotational semantics style *)
(* need to know as black box *)
module LensContext_Struct (Dict : Dictionary) : LensContext_Sig = struct
  type t = { defs     : (id, (lens*regex*regex)) Dict.t ;
             outgoing : (id, (lens*id) list) Dict.t     ;
             equivs   : id DisjointSet.t                ; }

  let empty = { defs     = Dict.empty        (=) ;
                outgoing = Dict.empty        (=) ;
                equivs   = DisjointSet.empty (=) ; }

  let lookup_exn (lc:t) (name:id) : lens*regex*regex =
    Dict.find_exn name lc.defs

  let lookup_type_exn (lc:t) (name:id) : regex*regex =
    let (_,r1,r2) = lookup_exn lc name in
    (r1,r2)

  let lookup_impl_exn (lc:t) (name:id) : lens =
    let (l,_,_) = lookup_exn lc name in
    l

  let update_defs (defs:(id, (lens*regex*regex)) Dict.t)
      (name:id) (l:lens) (r1:regex) (r2:regex)
    : (id, (lens*regex*regex)) Dict.t =
    begin match Dict.find name defs with
      | None -> Dict.set name (l,r1,r2) defs
      | Some _ -> failwith "bad insert"
    end

  let update_outgoing (outgoing:(id, (lens*id) list) Dict.t)
      (id1:id) (id2:id) (l:lens)
    : (id, (lens*id) list) Dict.t =
    let outgoing = begin match Dict.find id1 outgoing with
      | None -> Dict.set id1 [(l,id2)] outgoing
      | Some ol -> Dict.set id1 ((l,id2)::ol) outgoing
    end in
    let outgoing = begin match Dict.find id2 outgoing with
      | None -> Dict.set id2 [(LensInverse l,id1)] outgoing
      | Some ol -> Dict.set id2 ((LensInverse l,id1)::ol) outgoing
    end in
    outgoing

  let update_equivs (equivs:id DisjointSet.t) (id1:id) (id2:id)
    : id DisjointSet.t =
    DisjointSet.union_elements
      equivs
      id1
      id2

  (* TODO: is this the right thing, simpler if just between userdefs ? *)
  let insert_exn (lc:t) (name:id) (l:lens) (r1:regex) (r2:regex) : t =
    begin match (r1,r2) with
      | (RegExVariable id1, RegExVariable id2) ->
        { defs     = update_defs lc.defs name l r1 r2      ;
          outgoing = update_outgoing lc.outgoing id1 id2 (LensVariable name);
          equivs   = update_equivs lc.equivs id1 id2       ; }
      | _ -> 
        { defs     = update_defs lc.defs name l r1 r2 ;
          outgoing = lc.outgoing                      ;
          equivs   = lc.equivs                        ; }
    end

  let insert_list_exn (lc:t) (nirsl:(string * lens * regex * regex) list) : t =
    List.fold_left
      ~f:(fun acc (name,l,r1,r2) -> insert_exn acc name l r1 r2)
      ~init:lc
      nirsl

  let get_outgoing_edges (outgoing:(id, (lens*id) list) Dict.t) (source:id)
    : (lens * id) list =
    begin match Dict.find source outgoing with
      | None -> []
      | Some connections -> connections
    end

  let create_from_list_exn (nirsl:(string * lens * regex * regex) list) : t =
    insert_list_exn empty nirsl

  let shortest_path_exn (lc:t) (regex1_name:id) (regex2_name:id)
    : lens =
    let outgoing = lc.outgoing in
    let rec shortest_path_internal (accums:(lens * id) list) : lens =
      let satisfying_path_option =
        List.find
          ~f:(fun (_,n) -> n = regex2_name)
          accums
      in
      begin match satisfying_path_option with
        | None ->
          let accums =
            List.concat_map
              ~f:(fun (l,n) ->
                  let valid_outgoing_edges =
                    List.filter
                      ~f:(fun (l',_) -> not (has_common_sublens l' l))
                      (get_outgoing_edges outgoing n)
                  in
                  List.map
                    ~f:(fun (l',n') -> (LensCompose (l',l),n'))
                    valid_outgoing_edges)
              accums
          in
          shortest_path_internal accums
        | Some (l,_) -> l
      end
    in
    let regex1_rep = DisjointSet.find_representative lc.equivs regex1_name in
    let regex2_rep = DisjointSet.find_representative lc.equivs regex2_name in
    if regex1_rep <> regex2_rep then
      failwith "regexes not in same equivalence class"
    else if regex1_name = regex2_name then
      LensIdentity (RegExVariable regex1_name)
    else
      shortest_path_internal (get_outgoing_edges outgoing regex1_name)

  let shortest_path_to_rep_elt (lc:t) (regex_name:id) : id * lens =
    let rep_element = DisjointSet.find_representative lc.equivs regex_name in
    let shortest_path = shortest_path_exn lc regex_name rep_element in
    (rep_element,shortest_path)
end

module LensContext = LensContext_Struct(ListDictionary)

(***** }}} *****)
