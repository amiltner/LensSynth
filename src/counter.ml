open Core.Std
open Util

module type Counter_Sig = sig
  type 'a t

  val create : ('a -> 'a -> comparison) -> 'a t

  val add : 'a t -> 'a -> 'a t

  val merge : 'a t -> 'a t -> 'a t

  val compare_counts : ('a t) -> ('a t) -> comparison

  val multiply_merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  val as_ordered_assoc_list : 'a t -> ('a * int) list

  val map : ('a -> 'a) -> 'a t -> 'a t

  val map_unsafe : ('a -> 'a) -> 'a t -> 'a t

  val pp : ('a -> string) -> ('a t) -> string
end

module Counter : Counter_Sig = struct
  type 'a t = (('a * int) list * ('a -> 'a -> comparison))

  let create (comparer:'a -> 'a -> comparison) : 'a t =
    ([],comparer)

  let add ((counts,comparer):'a t) (data:'a) : 'a t =
    let rec add_internal (counts:('a * int) list) : ('a * int) list =
      begin match counts with
      | (h,i)::t ->
          begin match comparer data h with
          | LT -> (data,1)::counts
          | GT -> (h,i)::(add_internal t)
          | EQ -> (h,i+1)::t
          end
      | [] -> [data,1]
      end
    in
    (add_internal counts,comparer)

  let merge ((counts1,comparer1):'a t) ((counts2,comparer2):'a t) : 'a t =
    let comparer = comparer1 in
    let rec merge_internal (counts1:('a * int) list)
                           (counts2:('a * int) list)
                           : ('a * int) list =
      begin match (counts1,counts2) with
      | ((h1,i1)::t1,(h2,i2)::t2) ->
          begin match comparer h1 h2 with
          | EQ -> (h1,i1 + i2)::(merge_internal t1 t2)
          | LT -> (h1,i1 + 0)::(merge_internal t1 counts2)
          | GT -> (h2,0 + i2)::(merge_internal counts1 t2)
          end
      | (_,[]) -> counts1
      | ([],_) -> counts2
      end
    in
    
    if phys_equal comparer1 comparer2 then
      (merge_internal counts1 counts2,comparer1)
    else
      failwith "inconsistent comparers or identities"

  let compare_counts
              ((counts1,comparer1):'a t)
              ((counts2,comparer2):'a t)
              : comparison =
    let comparer = comparer1 in
    let rec compare_counts_internal
                (counts1:('a * int) list)
                (counts2:('a * int) list)
                : comparison =
      begin match (counts1,counts2) with
      | (((h1,i1)::t1),((h2,i2)::t2)) ->
          begin match comparer h1 h2 with
          | EQ ->
              if i1 < i2 then
                LT
              else if i1 > i2 then
                GT
              else
                compare_counts_internal t1 t2
          | x -> x
          end
      | ([],[]) -> EQ
      | ([],_::_) -> LT
      | (_::_,[]) -> GT
      end
    in

    if phys_equal comparer1 comparer2 then
      compare_counts_internal counts1 counts2
    else
      failwith "inconsistent comparers or identities"

  let correct_data_struct ((counts,comparer):'a t) : 'a t =
    let sorted_partitioned_cartesian_counts = sort_and_partition
      (fun (x,i) (y,i) -> comparer x y)
     counts 
    in
    (List.map
      ~f:(fun xil ->
        let (h,t) = split_by_first_exn xil in
        List.fold_left
          ~f:(fun (x,i) (_,j) -> (x,i+j))
          ~init:h
          t)
      sorted_partitioned_cartesian_counts, comparer)

  let multiply_merge (combiner:'a -> 'a -> 'a)
                     ((counts1,comparer1):'a t)
                     ((counts2,comparer2):'a t) : 'a t =
    let comparer = comparer1 in

    if phys_equal comparer1 comparer2 then
    (
      let cartesianed_counts =
        cartesian_map
          (fun (x1,i1) (x2,i2) -> ((combiner x1 x2),i1*i2))
          counts1
          counts2
      in

      correct_data_struct (cartesianed_counts,comparer)
      )
      else
        failwith "inconsistent comparers"
    
  let map_unsafe (f:'a -> 'a) ((counts,comparer):'a t) : 'a t =
    (List.map ~f:(fun (x,i) -> (f x,i)) counts, comparer)

  let map (f:'a -> 'a) (counts_structure:'a t) : 'a t =
    let mapped_unsafe_struct = map_unsafe f counts_structure in
    correct_data_struct mapped_unsafe_struct

  let as_ordered_assoc_list ((counts,_):'a t) : ('a * int) list =
    counts

  let pp (f:'a -> string) ((counts,_):'a t) : string =
    "[" ^ (String.concat ~sep:" ; " (List.map ~f:(fun (x,c) -> (f x) ^ "->" ^
    (string_of_int c)) counts)) ^ "]"
end

let counter_metric (comparer:'a -> 'a -> comparison) (identity:'a) (inner_metric:'a -> 'a -> int)
                   (x:'a Counter.t) (y:'a Counter.t) : int =
  let rec counter_metric_internal (l1:('a * int) list) (l2:('a * int) list)
    : int =
      begin match (l1,l2) with
      | ((h1,i1)::t1,(h2,i2)::t2) ->
          begin match comparer h1 h2 with
          | EQ ->
              if i1 = i2 then
                counter_metric_internal t1 t2
              else if i1 < i2 then
                counter_metric_internal t1 ((h2,i2-i1)::t2)
              else
                counter_metric_internal ((h1,i1-i2)::t1) t2
          | LT ->
              (i1 * (inner_metric h1 identity)) +
                (counter_metric_internal t1 l2)
          | GT ->
              (i2 * (inner_metric h2 identity)) +
                (counter_metric_internal l1 t2)
          end
      | (_,[]) ->
          List.fold_left
          ~f:(fun acc (h,i) ->
            acc+(i*(inner_metric h identity)))
          ~init:0
          l1
      | ([],_) ->
          List.fold_left
          ~f:(fun acc (h,i) ->
            acc+(i*(inner_metric identity h)))
          ~init:0
          l2
      end
  in
  counter_metric_internal (Counter.as_ordered_assoc_list x) (Counter.as_ordered_assoc_list y)
