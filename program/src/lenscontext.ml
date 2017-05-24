open Core
open Util
open String_utilities
open Lang

(***** The main LensContext module {{{ *****)

module type LensContext_Sig = sig
    type t

    val empty                    : t
    val lookup_exn               : t -> id -> (Lens.t * Regex.t * Regex.t)
    val lookup_type_exn          : t -> id -> Regex.t * Regex.t
    val lookup_impl_exn          : t -> id -> Lens.t
    val insert_exn               : t -> id -> Lens.t -> Regex.t -> Regex.t -> t
    val insert_list_exn          : t -> (id * Lens.t * Regex.t * Regex.t) list -> t
    val create_from_list_exn     : (id * Lens.t * Regex.t * Regex.t) list -> t
    val shortest_path            : t -> id -> id -> Lens.t option
    val shortest_path_exn        : t -> id -> id -> Lens.t
    val shortest_path_to_rep_elt : t -> id -> (id * Lens.t)
    val autogen_id_from_base     : t -> string -> id
    val autogen_id               : t -> Lens.t -> id
    val autogen_fresh_id         : t -> id
    val compare                  : t -> t -> comparison
    val to_string                : t -> string
    val hash                     : t -> int
end

(* add comments *)
(* declarative comments: google *)
(* SPECS not impl *)
(* here is the GOAL / meaning of what I'm doing *)
(* denotational semantics style *)
(* need to know as black box *)
module LensContext : LensContext_Sig = struct
  module DefsD = Dict.Make(
    struct
      type key = id
      type value = Lens.t * Regex.t * Regex.t
      let compare_key = compare_id
      let compare_value =
        triple_compare
          Lens.compare
          Regex.compare
          Regex.compare
      let key_to_string = show_id
      let value_to_string =
        string_of_triple
          Lens.show
          Regex.show
          Regex.show
    end)

  module OutgoingD = Dict.Make(
    struct
      type key = id
      type value = (Lens.t * id) list
      let compare_key = compare_id
      let compare_value =
        compare_list
          ~cmp:(pair_compare
             Lens.compare
             compare)
      let key_to_string = show_id
      let value_to_string =
        string_of_list
          (string_of_pair
             Lens.show
             show_id)
    end)

  module DS = Disjointset.Make(
    struct
      type elt = id
      let compare_elt = compare
      let elt_to_string = show_id
    end)

  type t = { defs     : DefsD.dict     ;
             outgoing : OutgoingD.dict ;
             equivs   : DS.set         ; }

  let empty = { defs     = DefsD.empty     ;
                outgoing = OutgoingD.empty ;
                equivs   = DS.empty        ; }

  let lookup_exn (lc:t) (name:id) : Lens.t*Regex.t*Regex.t =
    DefsD.lookup_exn lc.defs name

  let lookup_type_exn (lc:t) (name:id) : Regex.t*Regex.t =
    let (_,r1,r2) = lookup_exn lc name in
    (r1,r2)

  let lookup_impl_exn (lc:t) (name:id) : Lens.t =
    let (l,_,_) = lookup_exn lc name in
    l

  let update_defs (defs:DefsD.dict)
      (name:id) (l:Lens.t) (r1:Regex.t) (r2:Regex.t)
    : DefsD.dict =
    if not (DefsD.member defs name) then
      DefsD.insert defs name (l,r1,r2)
    else
      failwith "bad insert"

  let update_outgoing (outgoing:OutgoingD.dict)
      (id1:id) (id2:id) (l:Lens.t)
    : OutgoingD.dict =
    let outgoing = begin match OutgoingD.lookup outgoing id1 with
      | None -> OutgoingD.insert outgoing id1 [(l,id2)]
      | Some ol -> OutgoingD.insert outgoing id1 ((l,id2)::ol)
    end in
    let outgoing = begin match OutgoingD.lookup outgoing id2 with
      | None -> OutgoingD.insert outgoing id2 [(Lens.LensInverse l,id1)]
      | Some ol -> OutgoingD.insert outgoing id2 ((Lens.LensInverse l,id1)::ol)
    end in
    outgoing

  let update_equivs (equivs:DS.set) (id1:id) (id2:id)
    : DS.set =
    DS.union_elements
      equivs
      id1
      id2

  (* TODO: is this the right thing, simpler if just between userdefs ? *)
  let insert_exn (lc:t) (name:id) (l:Lens.t) (r1:Regex.t) (r2:Regex.t) : t =
    begin match (r1,r2) with
      | (Regex.RegExVariable id1, Regex.RegExVariable id2) ->
        { defs     = update_defs lc.defs name l r1 r2      ;
          outgoing = update_outgoing lc.outgoing id1 id2 (Lens.LensVariable name);
          equivs   = update_equivs lc.equivs id1 id2       ; }
      | _ -> 
        { defs     = update_defs lc.defs name l r1 r2 ;
          outgoing = lc.outgoing                      ;
          equivs   = lc.equivs                        ; }
    end

  let insert_list_exn (lc:t) (nirsl:(id * Lens.t * Regex.t * Regex.t) list) : t =
    List.fold_left
      ~f:(fun acc (name,l,r1,r2) -> insert_exn acc name l r1 r2)
      ~init:lc
      nirsl

  let get_outgoing_edges (outgoing:OutgoingD.dict) (source:id)
    : (Lens.t * id) list =
    begin match OutgoingD.lookup outgoing source with
      | None -> []
      | Some connections -> connections
    end

  let create_from_list_exn (nirsl:(id * Lens.t * Regex.t * Regex.t) list) : t =
    insert_list_exn empty nirsl

  let shortest_path (lc:t) (regex1_name:id) (regex2_name:id)
    : Lens.t option =
    let outgoing = lc.outgoing in
    let rec shortest_path_internal (accums:(Lens.t * id) list) : Lens.t =
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
                      ~f:(fun (l',_) -> not (Lens.has_common_sublens l' l))
                      (get_outgoing_edges outgoing n)
                  in
                  List.map
                    ~f:(fun (l',n') -> (Lens.LensCompose (l',l),n'))
                    valid_outgoing_edges)
              accums
          in
          shortest_path_internal accums
        | Some (l,_) -> l
      end
    in
    let regex1_rep = DS.find_representative lc.equivs regex1_name in
    let regex2_rep = DS.find_representative lc.equivs regex2_name in
    if regex1_rep <> regex2_rep then
      None
    else if regex1_name = regex2_name then
      Some (Lens.LensIdentity (Regex.RegExVariable regex1_name))
    else
      Some (shortest_path_internal (get_outgoing_edges outgoing regex1_name))

  let shortest_path_exn (lc:t) (regex1_name:id) (regex2_name:id)
    : Lens.t =
    begin match shortest_path lc regex1_name regex2_name with
      | None -> 
        failwith "regexes not in same equivalence class"
      | Some l -> l
    end

  let shortest_path_to_rep_elt (lc:t) (regex_name:id) : id * Lens.t =
    let rep_element = DS.find_representative lc.equivs regex_name in
    let shortest_path = shortest_path_exn lc regex_name rep_element in
    (rep_element,shortest_path)

  let autogen_id_from_base (lc:t) (base:string) : id =
    let rec fresh nopt =
      let (x,next) =
        begin match nopt with
          | None -> (base,Some 1)
          | Some n -> (Printf.sprintf "%s%d" base n, Some (n+1))
        end
      in
      if DefsD.member lc.defs (Id x) then
        fresh next
      else
        x
    in
    Id (fresh None)

  let autogen_id (lc:t) (l:Lens.t) : id =
    let base = Lens.show l in
    let rec fresh nopt =
      let (x,next) =
        begin match nopt with
          | None -> (base,Some 1)
          | Some n -> (Printf.sprintf "%s%d" base n, Some (n+1))
        end
      in
      begin match DefsD.lookup lc.defs (Id x) with
        | Some (l',_,_) ->
          if l = l' then
            x
          else
            fresh next
        | _ -> x
      end
    in
    Id (fresh None)
      
  let autogen_fresh_id (lc:t) : id =
    autogen_id_from_base lc "l"

  let compare (lc1:t) (lc2:t) : comparison =
    (* only need to compare defs as rest is just memoization *)
    DefsD.compare
      lc1.defs
      lc2.defs

  let to_string (lc:t) : string =
    DefsD.to_string lc.defs

  let hash (lc:t) : int =
    let kvp_list = DefsD.as_kvp_list lc.defs in
    List.foldi
      ~f:(fun i acc (id,(l,r1,r2)) ->
          (Regex.hash r1)
          lxor (Regex.hash r2)
          lxor (Lens.hash l)
          lxor (hash_id id)
          lxor (Int.hash i)
          lxor acc)
      ~init:(-25389029)
      kvp_list
end

(***** }}} *****)
