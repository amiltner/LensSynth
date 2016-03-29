open Core.Std

(*let rec cartesian_product (l:'a list list) : 'a list list =
  begin match l with
  | [] -> [[]]
  | x::xs -> let rest = cartesian_product xs in
    List.concat (List.map (fun i -> List.map (fun rs -> i :: rs) rest) x)
  end*)

let cartesian_map (f:'a -> 'b -> 'c) (l1:'a list) (l2:'b list) : 'c list =
  (List.fold_left
    ~f:(fun acc x ->
      (List.fold_left
        ~f:(fun acc2 y ->
          (f x y)::acc2)
        ~init:[]
        l2)@acc)
    ~init:[]
    l1)

let range (i:int) (j:int) : int list =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n::acc)
  in
  aux j []

type ('a,'b) either =
  | Left of 'a
  | Right of 'b

let distribute_option (l:('a option) list) : 'a list option =
  (List.fold_left
  ~f:(fun acc x ->
    begin match (acc,x) with
    | (None, _) -> None
    | (_, None) -> None
    | (Some acc', Some x') -> Some (x'::acc')
    end)
  ~init:(Some [])
  (List.rev l))

let rec time_action ~f:(f: unit -> 'a) : float * 'a =
  let t1  = Unix.gettimeofday () in
  let res = f () in
  let t2  = Unix.gettimeofday () in
  (t2 -. t1, res)

let rec lookup (k:'a) (l:('a * 'b) list) : 'b option =
  match l with
  | [] -> None
  | (k', v)::l -> if k = k' then Some v else lookup k l

let rec split_by_first_exn (l:'a list) : ('a * 'a list) =
  begin match l with
  | h::t -> (h,t)
  | [] -> failwith "need len at least 1"
  end

let split_by_last_exn (l:'a list) : 'a list * 'a =
  let (h,t) = split_by_first_exn (List.rev l) in
  (List.rev t, h)

let split_by_first_last_exn (l:'a list) : 'a * 'a list * 'a =
  let (h,t) = split_by_first_exn l in
  let (m,e) = split_by_last_exn t in
  (h,m,e)

let rec weld_lists (f: 'a -> 'a -> 'a) (l1:'a list) (l2:'a list) : 'a list =
  let (head,torso1) = split_by_last_exn l1 in
  let (torso2,tail) = split_by_first_exn l2 in
  head @ ((f torso1 torso2)::tail)

let duplicate (x:'a) (n:int) : 'a list =
  let rec duplicate_internal (x:'a) (n:int) (acc:'a list) : 'a list =
    if n = 0 then acc
    else duplicate_internal x (n-1) (x::acc)
  in
  duplicate_internal x n []

let bucketize_pairs (num_buckets:int) (data_position_pairs:('a * int) list) : ('a list) list =
  List.map
    ~f:(fun position -> List.filter_map
                        ~f:(fun (x,p) -> if position = p then
                                           Some x
                                         else
                                           None)
                        data_position_pairs)
    (range 0 (num_buckets-1))

let bucketize (f:'a -> int) (num_buckets:int) (l:'a list) : ('a list) list =
  let data_position_pairs = List.map
    ~f:(fun x -> (x,f x))
    l in
  bucketize_pairs num_buckets data_position_pairs

let attempt_bucketize (f:'a -> int option) (num_buckets:int) (l:'a list)
                   : ('a list) list option =
  let data_position_pairs_option = List.map
    ~f:(fun x -> begin match (f x) with
                 | None -> None
                 | Some y -> Some (x,y)
                 end)
    l in
  begin match (distribute_option data_position_pairs_option) with
  | None -> None
  | Some data_position_pairs ->
      Some (List.map
        ~f:(fun position -> List.filter_map
                            ~f:(fun (x,p) -> if position = p then
                                               Some x
                                             else
                                               None)
                            data_position_pairs)
        (range 0 num_buckets))
  end

let transpose_safe_empty_exn (row_count:int) (ls:'a list list) : 'a list list =
  if List.length ls = 0 then
    duplicate [] row_count
  else
    List.transpose_exn ls
