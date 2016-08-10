open Core.Std

module type Priority_Queue_Sig = sig
  type 'a t

  val create : 'a t
  val create_from_list : ('a * float) list -> 'a t
  val push : 'a t -> 'a -> float -> 'a t
  val push_all : 'a t -> ('a * float) list -> 'a t
  val pop : 'a t -> ('a * float * ('a t)) option
  val pp : ('a -> string) -> 'a t -> string
  val length : 'a t -> int
end

module Priority_Queue : Priority_Queue_Sig = struct
  type 'a t = ('a*float) list

  let create = []

  let length (queue:'a t) : int = List.length queue

  let push (queue:'a t) (data:'a) (priority:float) : 'a t =
    let rec push_internal (queue:'a t) (continuation:'a t -> 'a t) : 'a t =
      begin match queue with
      | (d,p)::t ->
          if p >= priority then
            continuation ((data,priority)::queue)
          else
            push_internal
              t
              (fun q -> continuation ((d,p)::q))
      | _ -> continuation [(data,priority)]
      end
    in
    push_internal queue (fun x -> x)

  let push_all (queue:'a t) (data_priority_list:('a * float) list) : 'a t =
    List.fold_left
    ~f:(fun acc (d,p) -> push acc d p)
      ~init:queue
      data_priority_list

  let create_from_list (data_priority_list:('a * float) list) : 'a t =
    push_all create data_priority_list

  let pop (queue:'a t) : ('a * float * ('a t)) option =
    begin match queue with
    | (d,p)::t -> Some (d,p,t)
    | [] -> None
    end

  let pp (f:'a -> string) (queue:'a t) : string =
    "[" ^
    (String.concat
      ~sep:";"
      (List.map
        ~f:(fun (d,p) ->
          "(" ^ (f d) ^ "," ^ (Float.to_string p) ^ ")")
        queue))
    ^ "]"
end
