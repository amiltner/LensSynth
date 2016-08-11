open Core.Std
open Util

module type Counters_Sig = sig
  type 'a t

  val create : ('a -> 'a -> comparison) -> 'a t

  val add : 'a t -> 'a -> 'a t

  val merge : (float -> float -> float) -> 'a t -> 'a t -> 'a t

  val multiply_merge : 'a t -> 'a t -> 'a t

  val as_ordered_assoc_list : 'a t -> ('a * float) list

  val pp : ('a -> string) -> ('a t) -> string
end

module Counters : Counters_Sig = struct
  type 'a t = (('a * float) list * ('a -> 'a -> comparison))

  let create (comparer:'a -> 'a -> comparison) : 'a t = ([],comparer)

  let add ((counts,comparer):'a t) (data:'a) : 'a t =
    let rec add_internal (counts:('a * float) list) : ('a * float) list =
      begin match counts with
      | (h,i)::t ->
          begin match comparer data h with
          | LT -> (data,1.0)::counts
          | GT -> (h,i)::(add_internal t)
          | EQ -> (h,i+.1.0)::t
          end
      | [] -> [data,1.0]
      end
    in
    (add_internal counts,comparer)

  let merge (combiner:float -> float -> float)
      ((counts1,comparer1):'a t) ((counts2,comparer2):'a t) : 'a t =
    let comparer = comparer1 in
    let rec merge_internal (counts1:('a * float) list)
                           (counts2:('a * float) list)
                           : ('a * float) list =
      begin match (counts1,counts2) with
      | ((h1,i1)::t1,(h2,i2)::t2) ->
          begin match comparer h1 h2 with
          | EQ -> (h1,combiner i1 i2)::(merge_internal t1 t2)
          | LT -> (h1,combiner i1 0.0)::(merge_internal t1 counts2)
          | GT -> (h2,combiner 0.0 i2)::(merge_internal counts1 t2)
          end
      | (_,[]) -> List.map ~f:(fun (d,i) -> (d,combiner i 0.0)) counts1
      | ([],_) -> List.map ~f:(fun (d,i) -> (d,combiner 0.0 i)) counts2
      end
    in
    
    if phys_equal comparer1 comparer2 then
      (merge_internal counts1 counts2,comparer1)
    else
      failwith "inconsistent comparers"


  let multiply_merge ((counts1,comparer1):'a t) ((counts2,comparer2):'a t) : 'a t =
    let comparer = comparer1 in
    let rec merge_internal (counts1:('a * float) list)
                           (counts2:('a * float) list)
                           : ('a * float) list =
      begin match (counts1,counts2) with
      | ((h1,i1)::t1,(h2,i2)::t2) ->
          begin match comparer h1 h2 with
          | EQ -> (h1,i1*.i2)::(merge_internal t1 t2)
          | LT -> (h1,i1)::(merge_internal t1 counts2)
          | GT -> (h2,i2)::(merge_internal counts1 t2)
          end
      | (_,[]) -> counts1
      | ([],_) -> counts2
      end
    in
    
    if phys_equal comparer1 comparer2 then
      (merge_internal counts1 counts2,comparer1)
    else
      failwith "inconsistent comparers"

  let as_ordered_assoc_list ((counts,_):'a t) : ('a * float) list =
    counts

  let pp (f:'a -> string) ((counts,_):'a t) : string =
    "[" ^ (String.concat ~sep:" ; " (List.map ~f:(fun (x,c) -> (f x) ^ "->" ^
    (Float.to_string c)) counts)) ^ "]"
end
