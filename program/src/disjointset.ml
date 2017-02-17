open Core.Std
open Util
open String_utilities

(***** The main RegexContext module {{{ *****)

module type DISJOINTSET =
sig
  type set
  type elt
    
  val empty                    : set
  val find_representative      : set -> elt -> elt
  val union_elements           : set -> elt -> elt -> set
  val create_from_equivalences : (elt * elt) list -> set
end

module type DISJOINTSET_ARG =
sig
  type elt
  val compare_elt : elt -> elt -> comparison
  val elt_to_string : elt -> string
end

module DictDisjointSet (DA : DISJOINTSET_ARG)
  : (DISJOINTSET with type elt = DA.elt) =
struct
  module D = Dict.Make(
    struct
      type key = DA.elt
      type value = DA.elt ref
      let compare_key = DA.compare_elt
      let compare_value = comparison_compare
      let key_to_string = DA.elt_to_string
      let value_to_string = (string_of_ref DA.elt_to_string)
    end)

  type set = D.dict
  type elt = DA.elt

  let empty = D.empty

  let rec find_representative (ds:set) (e:'a) : 'a =
    begin match D.lookup ds e with
      | None -> e
      | Some pref ->
        let rep = find_representative ds !pref in
        pref := rep;
        rep
    end

  let union_elements (ds:set) (e1:elt) (e2:elt) : set =
    let e1rep = find_representative ds e1 in
    let e2rep = find_representative ds e2 in
    if (e1rep = e2rep) then
      ds
    else
      D.insert ds e1rep (ref e2rep)

  let create_from_equivalences
      (equivs:(elt * elt) list) : set =
    List.fold_left
      ~f:(fun acc (e1,e2) ->
          union_elements acc e1 e2)
      ~init:(empty)
      equivs
end

module Make (D:DISJOINTSET_ARG) : (DISJOINTSET with type elt = D.elt) =
  DictDisjointSet(D)

(***** }}} *****)
