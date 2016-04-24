open Core.Std

type comparison =
    EQ
  | LT
  | GT

let int_to_comparison (n:int) : comparison =
  if n = 0 then
    EQ
  else if n < 0 then
    LT
  else
    GT

let comparison_to_int (c:comparison) : int =
  begin match c with
  | EQ -> 0
  | LT -> -1
  | GT -> 1
  end

let comparer_to_int_comparer (f:'a -> 'a -> comparison) (x:'a) (y:'a)
  : int =
    comparison_to_int (f x y)

let int_comparer_to_comparer (f:'a -> 'a -> int) (x:'a) (y:'a)
  : comparison =
    int_to_comparison (f x y)

let comparison_compare = fun x y -> int_to_comparison (compare x y)

let compare_ints (n1:int) (n2:int) : comparison =
  int_to_comparison (compare n1 n2)

type ('a, 'b) either =
    Left of 'a
  | Right of 'b

let rec fold_until_completion (f: 'a -> ('a,'b) either) (acc:'a) : 'b =
  begin match f acc with
  | Left acc' -> fold_until_completion f acc'
  | Right answer -> answer
  end

let cartesian_map (f:'a -> 'b -> 'c) (l1:'a list) (l2:'b list) : 'c list =
  (List.fold_right
    ~f:(fun x acc ->
      (List.fold_right
        ~f:(fun y acc2 ->
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

let rec split_by_first_satisfying (f:'a -> bool) (l:'a list)
                            : ('a * 'a list) option =
  begin match l with
  | [] -> None
  | h::t -> if f h then
              Some (h,t)
            else
              begin match split_by_first_satisfying f t with
              | None -> None
              | Some (h',t') -> Some (h',h::t')
              end
  end

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

let split_at_index_exn (l:'a list) (i:int) : 'a list * 'a list =
  let rec split_at_index_exn_internal (l:'a list) (i:int)
            (continuation:('a list * 'a list) -> ('a list * 'a list))
          : 'a list * 'a list =
    begin match (l,i) with
    | (_,0) -> continuation ([],l)
    | (h::t,_) ->
        split_at_index_exn_internal t (i-1)
            (fun (l1,l2) -> continuation (h::l1,l2)) 
    | _ -> failwith "index out of range"
    end in
  if i < 0 then
    failwith "invalid index"
  else
    split_at_index_exn_internal l i (fun x -> x)

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

let is_prime (n:int) : bool =
  let rec loop (k:int) : bool =
    if k*k > n then
      true
    else if n mod k = 0 then
      false
    else
      loop (k+2)
  in
  if n=2 then
    true
  else if n < 2 || n mod 2 = 0 then
    false
  else
    loop 3

let primes_beneath_n (n:int) : int list =
  List.filter
  ~f:is_prime
  (range 0 (n))

let rec sort_and_partition (f:'a -> 'a -> comparison) (l:'a list) : 'a list list =
  let rec merge_sorted_partitions (l1:'a list list) (l2:'a list list) : 'a list list =
    begin match (l1,l2) with
    | (h1::t1,h2::t2) ->
        let rep1 = List.hd_exn h1 in
        let rep2 = List.hd_exn h2 in
        let comparison = f rep1 rep2 in
        begin match comparison with
        | EQ -> ((h1@h2)::(merge_sorted_partitions t1 t2))
        | LT -> (h1::(merge_sorted_partitions t1 l2))
        | GT -> (h2::(merge_sorted_partitions l1 t2))
        end
    | _ -> l1 @ l2
    end in
  begin match l with
  | [] -> []
  | [h] -> [[h]]
  | _ ->
      let len = List.length l in
      let (l1, l2) = split_at_index_exn l (len/2) in
      let sorted_partitioned_l1 = sort_and_partition f l1 in
      let sorted_partitioned_l2 = sort_and_partition f l2 in
      merge_sorted_partitions sorted_partitioned_l1 sorted_partitioned_l2
  end

let sort_and_partition_with_indices (f:'a -> 'a -> comparison)
                        (l:'a list) : ('a * int) list list =
  (*let rec merge_sorted_partitions (l1:('a * int) list list)
                (l2:('a * int) list list) : ('a * int) list list =
    begin match (l1,l2) with
    | (h1::t1,h2::t2) ->
        let (rep1,_) = List.hd_exn h1 in
        let (rep2,_) = List.hd_exn h2 in
        let comparison = f rep1 rep2 in
        begin match comparison with
        | EQ -> ((h1@h2)::(merge_sorted_partitions t1 t2))
        | LT -> (h1::(merge_sorted_partitions t1 l2))
        | GT -> (h2::(merge_sorted_partitions l1 t2))
        end
    | _ -> l1 @ l2
    end in
  let rec sort_and_partition_with_indices_internal (l:('a * int) list)
                      : ('a * int) list list =*)
  let rec merge_grouped_things (remaining:('a * int) list) (currentacc:('a*int) list)
  (accacc:('a*int) list list) : ('a*int) list list =
    begin match remaining with
    | [] -> currentacc :: accacc
    | (h,i)::t -> let currenthd = fst (List.hd_exn currentacc) in
      begin match f h currenthd with
      | EQ -> merge_grouped_things t ((h,i)::currentacc) accacc
      | _ -> merge_grouped_things t [(h,i)] (currentacc::accacc)
      end
    end
  in


  let sorted = List.sort
    ~cmp:(fun (x,_) (y,_) -> comparison_to_int (f x y))
    (List.mapi ~f:(fun i x -> (x,i)) l) in

  begin match sorted with
  | [] -> []
  | h::t -> merge_grouped_things t [h] []
  end

    (*begin match l with
    | [] -> []
    | [h] -> [[h]]
    | _ ->
        let len = List.length l in
        let (l1, l2) = split_at_index_exn l (len/2) in
        let sorted_partitioned_l1 = sort_and_partition_with_indices_internal l1 in
        let sorted_partitioned_l2 = sort_and_partition_with_indices_internal l2 in
        merge_sorted_partitions sorted_partitioned_l1 sorted_partitioned_l2
    end in
  sort_and_partition_with_indices_internal
    (List.mapi ~f:(fun i x -> (x,i)) l)*)

let ordered_partition_order (f:'a -> 'a -> comparison)
                            (l1:'a list) (l2:'a list)
                            : comparison =
  let p1 = sort_and_partition f l1 in
  let p2 = sort_and_partition f l2 in
  begin match (compare_ints (List.length p1) (List.length p2)) with
  | EQ ->
      List.fold_left
      ~f:(fun acc (l1',l2') ->
        begin match acc with
        | EQ ->
            begin match (compare_ints (List.length l1') (List.length l2')) with
            | EQ -> f (List.hd_exn l1') (List.hd_exn l2')
            | c -> c
            end
        | c -> c
        end)
      ~init:EQ
      (List.zip_exn p1 p2)
  | c -> c
  end

let rec dictionary_order (f:'a -> 'a -> comparison)
  (l1:'a list) (l2:'a list) : comparison =
    begin match (l1,l2) with
    | ([],[]) -> EQ
    | (_::_,[]) -> GT
    | ([],_::_) -> LT
    | (h1::t1,h2::t2) ->
        begin match f h1 h2 with
        | EQ -> dictionary_order f t1 t2
        | x -> x
        end
    end

let partition_dictionary_order (f:'a -> 'a -> comparison)
  : 'a list list -> 'a list list -> comparison =
    dictionary_order
      (fun x y -> f (List.hd_exn x) (List.hd_exn y))

let ordered_partition_dictionary_order (f:'a -> 'a -> comparison)
  : ('a * int) list list -> ('a * int) list list -> comparison =
    dictionary_order
      (fun x y ->
        begin match int_to_comparison (compare (List.length x) (List.length y)) with
        | EQ -> f (fst (List.hd_exn x)) (fst (List.hd_exn y))
        | x -> x
        end)

let rec intersect_ordered_no_dupes (cmp:'a -> 'a -> comparison)
                                   (l1:'a list) (l2:'a list)
                                   : 'a list =
  begin match (l1,l2) with
  | (h1::t1,h2::t2) ->
      begin match (cmp h1 h2) with
      | EQ -> h1::(intersect_ordered_no_dupes cmp t1 t2)
      | LT -> intersect_ordered_no_dupes cmp t1 l2
      | GT -> intersect_ordered_no_dupes cmp l1 t2
      end
  | ([],_) -> []
  | (_,[]) -> []
  end

let intersect_lose_order_no_dupes (cmp:'a -> 'a -> comparison)
                                  (l1:'a list) (l2:'a list)
                                  : 'a list =
  let int_comparer = comparer_to_int_comparer cmp in
  let ordered_l1 = List.sort ~cmp:int_comparer l1 in
  let ordered_l2 = List.sort ~cmp:int_comparer l2 in
  intersect_ordered_no_dupes cmp ordered_l1 ordered_l2

let set_minus_lose_order (cmp:'a -> 'a -> comparison)
                                  (l1:'a list) (l2:'a list)
                                  : 'a list =
  let rec set_minus_ordered (l1:'a list) (l2:'a list) : 'a list =
    begin match (l1,l2) with
    | (h1::t1,h2::t2) ->
        begin match (cmp h1 h2) with
        | EQ -> set_minus_ordered t1 t2
        | LT -> h1::(set_minus_ordered t1 l2)
        | GT -> set_minus_ordered l1 t2
        end
    | ([],_) -> []
    | (_,[]) -> []
    end
  in
  let int_comparer = comparer_to_int_comparer cmp in
  let ordered_l1 = List.dedup (List.sort ~cmp:int_comparer l1) in
  let ordered_l2 = List.dedup (List.sort ~cmp:int_comparer l2) in
  set_minus_ordered ordered_l1 ordered_l2

let discrete_metric (cmp:'a -> 'a -> comparison)
                    (x:'a) (y:'a)
                    : int =
  begin match cmp x y with
  | EQ -> 1
  | _ -> 0
  end
