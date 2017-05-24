open Core
open Util
open String_utilities

module type PRIORITY_QUEUE = sig
  type queue
  type element

  val empty : queue
  val from_list : element list -> queue
  val singleton : element -> queue
  val push : queue -> element -> queue
  val push_all : queue -> element list -> queue
  val pop : queue -> (element * int * queue) option
  val pop_exn : queue -> (element * int * queue)
  val pop_until_min_pri_greater_than : queue -> int -> ((element * int) list * queue)
  val all_remaining : queue -> (element * int) list
  val length : queue -> int
  val compare : queue -> queue -> comparison
  val to_string : queue -> string
end

module type PRIORITY_QUEUE_ARG =
sig
  type element
  val compare : element -> element -> comparison
  val priority : element -> int
  val to_string : element -> string
end

module HeapSetQueue(P:PRIORITY_QUEUE_ARG)
  : (PRIORITY_QUEUE with type element = P.element) =
struct
  module QueueHeap =
    Comparison_heap.Make(
    struct
      type element = (P.element * int)
      let compare =
        (fun (_,f1) (_,f2) ->
             (compare_int f1 f2))
      let to_string = (string_of_pair P.to_string string_of_int)
    end)

  module PushedSet =
    Comparison_set.Make(
    struct
      type element = P.element
      let compare = P.compare
      let to_string = P.to_string
    end)

  type queue = QueueHeap.heap * PushedSet.set
  type element = P.element

  let empty = (QueueHeap.empty, PushedSet.empty)

  let push ((h,s):queue) (e:element) : queue =
    if PushedSet.member s e then
      (h,s)
    else
      let s' = PushedSet.insert e s in
      let pri = P.priority e in
      let h' = QueueHeap.push h (e,pri) in
      (h',s')

  let push_all (q:queue) (es:element list) : queue =
    List.fold_left
      ~f:(fun q e -> push q e)
      ~init:q
      es

  let from_list (es:element list) : queue =
    push_all empty es

  let singleton (e:element) : queue =
    from_list [e]

  let pop ((h,s):queue) : ('a * int * queue) option =
    Option.map ~f:(fun ((e,p),h') -> (e,p,(h',s))) (QueueHeap.pop h)

  let pop_exn (q:queue) : 'a * int * queue =
    begin match pop q with
      | None -> failwith "failure: pop_exn"
      | Some e -> e
    end

  let all_remaining ((h,_):queue) : ('a * int) list =
    QueueHeap.to_list h

  let rec pop_until_min_pri_greater_than
      (q:queue)
      (f:int)
    : (element * int) list * queue =
      begin match pop q with
        | None -> ([],q)
        | Some (e,f',q') ->
          if f' > f then
            ([],q)
          else
            let (efs,q'') = pop_until_min_pri_greater_than q' f in
            ((e,f')::efs,q'')
      end


  let length ((h,_):queue) : int = QueueHeap.size h

  let to_string : queue -> string =
    string_of_pair
      QueueHeap.to_string
      PushedSet.to_string

  let compare : queue -> queue -> comparison =
    pair_compare
      QueueHeap.compare
      PushedSet.compare
end

module Make (Q:PRIORITY_QUEUE_ARG) : (PRIORITY_QUEUE with type element = Q.element) =
  HeapSetQueue(Q)
