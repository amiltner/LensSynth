open Core.Std
open Lang
open Util
open Lens
open Datastructures

(***** The main RegexContext module {{{ *****)

module type DisjointSet_Sig = sig
    type 'a t

    val empty                    : ('a -> 'a -> bool) -> 'a t
    val find_representative      : 'a t -> 'a -> 'a
    val union_elements           : 'a t -> 'a -> 'a -> 'a t
    val create_from_equivalences : ('a -> 'a -> bool) -> ('a * 'a) list -> 'a t
end

module DisjointSet_Struct (Dict : Dictionary) : DisjointSet_Sig = struct
  type 'a t = ('a, 'a ref) Dict.t

  let empty comparer = Dict.empty comparer

  let rec find_representative (ds:'a t) (e:'a) : 'a =
    begin match Dict.find e ds with
      | None -> e
      | Some pref ->
        let rep = find_representative ds !pref in
        pref := rep;
        rep
    end

  let union_elements (ds:'a t) (e1:'a) (e2:'a) : 'a t =
    let e1rep = find_representative ds e1 in
    let e2rep = find_representative ds e2 in
    Dict.set e1rep (ref e2rep) ds

  let create_from_equivalences (comparer:'a -> 'a -> bool)
      (equivs:('a * 'a) list) : 'a t =
    List.fold_left
      ~f:(fun acc (e1,e2) ->
          union_elements acc e1 e2)
      ~init:(empty (=))
      equivs
end

module DisjointSet = DisjointSet_Struct(ListDictionary)

(***** }}} *****)
