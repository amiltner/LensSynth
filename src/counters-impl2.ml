open Core.Std
open Tree
open Util

module type Counters_Sig = sig
  type 'a t

  val create_from_datum : ('a -> 'a -> comparison) -> 'a -> 'a t

  val create_from_data : ('a -> 'a -> comparison) -> 'a list -> 'a t

  val merge_unsafe : ('a t) list -> 'a t

  val as_ordered_assoc_list : 'a t -> ('a * int) list

  val pp : ('a -> string) -> ('a t) -> string
end

module Counters : Counters_Sig = struct
  type 'a t = (('a Tree.t) * ('a -> 'a -> comparison))

  let create_from_datum (comparer:'a -> 'a -> comparison) (d:'a) : 'a t =
    (Leaf d,comparer)

  let create_from_data (comparer:'a -> 'a -> comparison) (dl:'a list) : 'a t =
    (Node (List.map ~f:(fun d -> Leaf d) dl), comparer)

  let merge_unsafe (tcl:('a t) list) : 'a t =
    let comparer = comparison_compare in
    (Node (List.map ~f:fst tcl), comparer)


  let as_ordered_assoc_list ((counts,comparer):'a t) : ('a * int) list =
    let rec retrieve_counts_in_ordered_list (counts:'a list)
                                            (acc:('a * int) list)
                                            : ('a * int) list =
      begin match counts with
      | [] -> acc
      | h::t ->
          retrieve_counts_in_ordered_list
            t
            (begin match acc with
            | [] -> [(h,1)]
            | (hd,i)::tl ->
                begin match comparer hd h with
                | EQ -> (hd,i+1)::tl
                | _ -> (h,1)::acc
                end
            end)
      end
    in
    let data_list = Tree.to_list counts in
    print_endline "get here?";
    let ordered_data_list = List.sort ~cmp:(comparer_to_int_comparer comparer) data_list in
    print_endline "am i";
     let x = List.rev (retrieve_counts_in_ordered_list ordered_data_list []) in
     print_endline "slower?";
     x

  let pp (f:'a -> string) (counts:'a t) : string = 
    "[" ^ (String.concat ~sep:" ; " (List.map ~f:(fun (x,c) -> (f x) ^ "->" ^
    (string_of_int c)) (as_ordered_assoc_list counts))) ^ "]"
end
