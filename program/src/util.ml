open Core.Std

let random_char () =
  let random_int = Random.int 256 in
  Char.of_int_exn random_int

let (%) (f:'b -> 'c) (g:'a -> 'b) : 'a -> 'c =
  Fn.compose f g

type comparison =
    EQ
  | LT
  | GT

type partial_order_comparison =
    PO_EQ
  | PO_LT
  | PO_GT
  | PO_INCOMPARABLE

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

let comparison_to_bool (c:comparison) : bool =
  begin match c with
    | EQ -> true
    | _ -> false
  end

let comparer_to_int_comparer (f:'a -> 'a -> comparison) (x:'a) (y:'a)
  : int =
    comparison_to_int (f x y)

let int_comparer_to_comparer (f:'a -> 'a -> int) (x:'a) (y:'a)
  : comparison =
  int_to_comparison (f x y)

let comparer_to_equality_check (f:'a -> 'a -> comparison) (x:'a) (y:'a)
  : bool =
  comparison_to_bool (f x y)

let comparison_compare = fun x y -> int_to_comparison (compare x y)

let compare_ints (n1:int) (n2:int) : comparison =
  int_to_comparison (compare n1 n2)

let pp_comparison (c:comparison) : string =
  begin match c with
  | EQ -> "EQ"
  | LT -> "LT"
  | GT -> "GT"
  end

type ('a, 'b) either =
    Left of 'a
  | Right of 'b

let rec fold_until_completion (f: 'a -> ('a,'b) either) (acc:'a) : 'b =
  begin match f acc with
  | Left acc' -> fold_until_completion f acc'
  | Right answer -> answer
  end

let fold_until_fixpoint (f:'a -> 'a) : 'a -> 'a =
  fold_until_completion
    (fun x ->
       let x' = f x in
       if x = x' then
         Right x
       else
         Left x')

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
  aux (j-1) []

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

let swap_double ((x,y):'a * 'b) : 'b * 'a =
  (y,x)

let time_action ~f:(f: unit -> 'a) : float * 'a =
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

let split_by_first (l:'a list) : ('a * 'a list) option =
  begin match l with
    | h::t -> Some (h,t)
    | [] -> None
  end

let split_by_first_exn (l:'a list) : ('a * 'a list) =
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

let fold_on_head_exn (f:'a -> 'a -> 'a) (l:'a list) : 'a =
  let (h,t) = split_by_first_exn l in
  List.fold_left
    ~f:f
    ~init:h
    t

let fold_on_head (f:'a -> 'a -> 'a) (l:'a list) : 'a option =
  begin match l with
    | [] -> None
    | _ -> Some (fold_on_head_exn f l)
  end

let fold_on_head_with_default (f:'a -> 'a -> 'a) (d:'a) (l:'a list) : 'a =
  begin match l with
    | [] -> d
    | _ -> fold_on_head_exn f l
  end

let weld_lists (f: 'a -> 'a -> 'a) (l1:'a list) (l2:'a list) : 'a list =
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
    (range 0 (num_buckets))

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

let primes_between (n:int) (m:int) : int list =
  List.filter
  ~f:is_prime
  (range n m)

let rec partitions (n:int) (k:int) : int list list =
  if n <= 0 || k <= 0 then
    []
  else if k = 1 then
    [[n]]
  else
    List.fold_left ~f:(fun res i ->
      List.append res @@ List.map ~f:(fun t -> i::t) (partitions (n-i) (k-1)))
      ~init:[] (List.map ~f:((+) 1) (range 0 (n-k+1)))

let double_partitions (n:int) : (int * int) list =
  let list_split_partitions = partitions n 2 in
  List.map
    ~f:(fun pl ->
        begin match pl with
          | [f;s] -> (f,s)
          | _ -> failwith "bug in double_partitions"
        end)
    list_split_partitions

let triple_partitions (n:int) : (int * int * int) list =
  let list_split_partitions = partitions n 3 in
  List.map
    ~f:(fun tl ->
        begin match tl with
          | [f;s;t] -> (f,s,t)
          | _ -> failwith "bug in triple_partitions"
        end)
    list_split_partitions

let sort ~(cmp:('a -> 'a -> comparison)) (l:'a list) : 'a list =
  let int_comparer = comparer_to_int_comparer cmp in
  List.sort ~cmp:int_comparer l

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

let option_compare
    (value_compare:'a -> 'a -> comparison)
    (xo:'a option)
    (yo:'a option)
  : comparison =
  begin match (xo,yo) with
    | (None, None) -> EQ
    | (None, Some _) -> LT
    | (Some _, None) -> GT
    | (Some x, Some y) -> value_compare x y
  end

let either_compare
    (left_compare:'a -> 'a -> comparison)
    (right_compare:'a -> 'a -> comparison)
    (xe:('a,'b) either)
    (ye:('a,'b) either)
  : comparison =
  begin match (xe,ye) with
    | (Left xl, Left yl) ->
      left_compare xl yl
    | (Left _, _) ->
      LT
    | (Right xr, Right yr) ->
      right_compare xr yr
    | (Right _, _) -> GT
  end

let pair_compare
    (fst_compare:'a -> 'a -> comparison)
    (snd_compare:'b -> 'b -> comparison)
    ((x1,x2):('a * 'b))
    ((y1,y2):('a * 'b))
  : comparison =
  begin match fst_compare x1 y1 with
    | EQ ->
      snd_compare x2 y2
    | c -> c
  end

let triple_compare
    (fst_compare:'a -> 'a -> comparison)
    (snd_compare:'b -> 'b -> comparison)
    (trd_compare:'c -> 'c -> comparison)
    ((x1,x2,x3):('a * 'b * 'c))
    ((y1,y2,y3):('a * 'b * 'c))
  : comparison =
  begin match fst_compare x1 y1 with
    | EQ ->
      begin match snd_compare x2 y2 with
        | EQ ->
          trd_compare x3 y3
        | c -> c
      end
    | c -> c
  end

let quad_compare
    (fst_compare:'a -> 'a -> comparison)
    (snd_compare:'b -> 'b -> comparison)
    (trd_compare:'c -> 'c -> comparison)
    (rth_compare:'d -> 'd -> comparison)
    ((x1,x2,x3,x4):('a * 'b * 'c * 'd))
    ((y1,y2,y3,y4):('a * 'b * 'c * 'd))
  : comparison =
  begin match fst_compare x1 y1 with
    | EQ ->
      begin match snd_compare x2 y2 with
        | EQ ->
          begin match trd_compare x3 y3 with
            | EQ -> rth_compare x4 y4
            | c -> c
          end
        | c -> c
      end
    | c -> c
  end

let quint_compare
    (fst_compare:'a -> 'a -> comparison)
    (snd_compare:'b -> 'b -> comparison)
    (trd_compare:'c -> 'c -> comparison)
    (rth_compare:'d -> 'd -> comparison)
    (fth_compare:'e -> 'e -> comparison)
    ((x1,x2,x3,x4,x5):('a * 'b * 'c * 'd * 'e))
    ((y1,y2,y3,y4,y5):('a * 'b * 'c * 'd * 'e))
  : comparison =
  begin match fst_compare x1 y1 with
    | EQ ->
      begin match snd_compare x2 y2 with
        | EQ ->
          begin match trd_compare x3 y3 with
            | EQ ->
              begin match rth_compare x4 y4 with
                | EQ -> fth_compare x5 y5
                | c -> c
              end
            | c -> c
          end
        | c -> c
      end
    | c -> c
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

let intersect_lose_order_no_dupes (cmp:'a -> 'a -> comparison)
                                  (l1:'a list) (l2:'a list)
                                  : 'a list =
  let rec intersect_ordered (l1:'a list) (l2:'a list) : 'a list =
    begin match (l1,l2) with
    | (h1::t1,h2::t2) ->
        begin match (cmp h1 h2) with
        | EQ -> h1::(intersect_ordered t1 t2)
        | LT -> intersect_ordered t1 l2
        | GT -> intersect_ordered l1 t2
        end
    | ([],_) -> []
    | (_,[]) -> []
    end
  in
  let int_comparer = comparer_to_int_comparer cmp in
  let ordered_l1 = List.sort ~cmp:int_comparer l1 in
  let ordered_l2 = List.sort ~cmp:int_comparer l2 in
  intersect_ordered ordered_l1 ordered_l2

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
    | (_,[]) -> l1
    end
  in
  let int_comparer = comparer_to_int_comparer cmp in
  let ordered_l1 = List.dedup (List.sort ~cmp:int_comparer l1) in
  let ordered_l2 = List.dedup (List.sort ~cmp:int_comparer l2) in
  set_minus_ordered ordered_l1 ordered_l2

let pairwise_maintain_invariant
        (invariant:'a -> 'a -> bool)
        (l1:'a list)
        (l2:'a list)
        : bool =
  List.for_all
    ~f:(fun x ->
      List.for_all
        ~f:(invariant x)
        l2)
    l1

let rec zip_nondist (xs:'a list) (ys:'b list) : (('a option * 'b option) list) =
  begin match (xs,ys) with
  | (x::xs,y::ys) -> (Some x, Some y)::(zip_nondist xs ys)
  | ([],_) -> List.map ~f:(fun y -> (None, Some y)) ys
  | (_,[]) -> List.map ~f:(fun x -> (Some x, None)) xs
  end

let rec zip_with  (xs:'a list)
                  (ys:'b list)
                  (f_match:'a -> 'b -> 'c)
                  (unmatch_left:'a -> 'c)
                  (unmatch_right:'b -> 'c)
                  : 'c list =
  begin match (xs,ys) with
  | (h1::t1,h2::t2) ->
      (f_match h1 h2)::(zip_with t1 t2 f_match unmatch_left unmatch_right)
  | (_,[]) -> List.map ~f:unmatch_left xs
  | ([],_) -> List.map ~f:unmatch_right ys
  end


let rec assoc_value_mem (value:'b) (l:('a * 'b) list) : 'a option =
  begin match l with
  | (k,v)::t -> if value = v then Some k else assoc_value_mem value t
  | [] -> None
  end


type ('a,'b) tagged_list_tree =
  | Leaf of 'b
  | Node of 'a * ((('a,'b) tagged_list_tree) list)

let rec insert_into_correct_list (l:('a * 'b list) list) (k:'a) (v:'b)
    : ('a * 'b list) list =
  begin match l with
  | ((k',vlist)::kvplist) ->
      if k = k' then
        (k',v::vlist)::kvplist
      else
        (k',vlist)::(insert_into_correct_list kvplist k v)
  | [] -> failwith "bad list"
  end

let rec append_into_correct_list ((k,v):'a * 'b list) (l:('a * 'b list) list)
    : ('a * 'b list) list =
  begin match l with
  | ((k',vlist)::kvplist) ->
      if k = k' then
        (k',v@vlist)::kvplist
      else
        (k',vlist)::(append_into_correct_list (k,v) kvplist)
  | [] -> failwith "bad lisat"
  end

let group_by_values (l:('a list * 'b) list) : ('a list * 'b) list =
  let empty_value_list = List.dedup (List.map ~f:(fun v -> (snd v,[])) l) in
  let l' = List.fold_left
  ~f:(fun acc (k,v) ->
    append_into_correct_list (v,k) acc)
  ~init:empty_value_list
  l
  in
  List.map ~f:(fun (x,y) -> (y,x)) l'

let group_by_keys (kvl:('a * 'b) list) : ('a * 'b list) list =
  let empty_key_list = List.dedup (List.map ~f:(fun v -> (fst v,[])) kvl) in
  List.fold_left
    ~f:(fun acc (k,v) ->
        insert_into_correct_list acc k v)
    ~init:empty_key_list
    kvl


let rec tagged_list_tree_keygrouped (tlt:('a,'b) tagged_list_tree) : ('a
list,'b) tagged_list_tree =
  begin match tlt with
  | Leaf v -> Leaf v
  | Node (k,tltl) ->
      let is_noded = (begin match List.hd_exn tltl with
          | Leaf _ -> false
          | Node _ -> true
      end) in
      let tltl' = List.map ~f:tagged_list_tree_keygrouped tltl in
      if is_noded then
        Node ([k],handle_noded_tltl tltl')
      else
        Node ([k],tltl')
  end

and handle_noded_tltl (tltl:('a list,'b) tagged_list_tree list) : ('a list,'b)
tagged_list_tree list =
  let kvps = List.map ~f:(fun tlt -> begin match tlt with
              | Leaf _ -> failwith "bad"
              | Node (k,tltl) -> (k,tltl)
              end) tltl in
  let kvpg = group_by_values kvps in
  List.map ~f:(fun (k,tltl) -> Node (k,tltl)) kvpg

module Operators = struct 
    let (>?>) (x : 'a option) (f : 'a -> 'b option) : 'b option = match x with
      | None -> None
      | Some v -> f v
end

let maximally_factor_hemiring_element
    (apply_to_every_level:('a -> 'a) -> ('a -> 'a))
    (multiplicative_identity:'a)
    (separate_plus:'a -> ('a * 'a) option)
    (separate_times:'a -> ('a * 'a) option)
    (create_plus:'a -> 'a -> 'a)
    (create_times:'a -> 'a -> 'a)
  : 'a -> 'a =
  let rec separate_into_sum
      (r:'a)
    : 'a list =
    begin match separate_plus r with
      | None -> [r]
      | Some (r1,r2) -> (separate_into_sum r1) @ (separate_into_sum r2)
    end
  in
  let rec separate_into_product
      (r:'a)
    : 'a list =
    begin match separate_times r with
      | None -> [r]
      | Some (r1,r2) -> (separate_into_product r1) @ (separate_into_product r2)
    end
  in
  let combine_nonempty_list_exn
      (combiner:'a -> 'a -> 'a)
      (rl:'a list)
    : 'a =
    let (rlf,rll) = split_by_last_exn rl in
    List.fold_right
      ~f:(fun r acc ->
          combiner r acc)
      ~init:rll
      rlf
  in
  let combine_list
      (combiner:'a -> 'a -> 'a)
      (rl:'a list)
    : 'a option =
    begin match rl with
      | [] -> None
      | _ -> Some (combine_nonempty_list_exn combiner rl)
    end
  in
  let maximally_factor_current_level
      (product_splitter:'a list -> ('a * 'a list))
      (product_combiner:'a -> 'a -> 'a)
      (he:'a)
    : 'a =
    let sum_list = separate_into_sum he in
    let sum_product_list_list =
      List.map
        ~f:separate_into_product
        sum_list
    in
    let product_keyed_sum_list =
      List.map
        ~f:product_splitter
        sum_product_list_list
    in
    let grouped_assoc_list = group_by_keys product_keyed_sum_list in
    let keyed_sum_list =
      List.map
        ~f:(fun (k,all) ->
            let producted_elements =
              List.map
                ~f:(fun pl ->
                    begin match combine_list create_times pl with
                      | None -> multiplicative_identity
                      | Some he -> he
                    end)
                all
            in
            (k,producted_elements))
        grouped_assoc_list
    in
    let ringed_list =
      List.map
        ~f:(fun (k,al) ->
            let factored_side = combine_nonempty_list_exn create_plus al in
            if factored_side = multiplicative_identity then
              k
            else
              product_combiner
                k
                factored_side)
        keyed_sum_list
    in
    combine_nonempty_list_exn create_plus ringed_list
  in
  Fn.compose
    (fold_until_fixpoint
       (apply_to_every_level
          (maximally_factor_current_level
             (Fn.compose swap_double split_by_last_exn)
             (Fn.flip create_times))))
    (fold_until_fixpoint
       (apply_to_every_level
          (maximally_factor_current_level
             split_by_first_exn
             create_times)))

let string_to_char_list (s:string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let hash_pair
    (fst_hash:'a -> int)
    (snd_hash:'b -> int)
    ((a,b):'a * 'b)
  : int =
  (fst_hash a) lxor (snd_hash b)

let hash_triple
    (fst_hash:'a -> int)
    (snd_hash:'b -> int)
    (trd_hash:'c -> int)
    ((a,b,c):'a * 'b * 'c)
  : int =
  (fst_hash a) lxor (snd_hash b) lxor (trd_hash c)

let hash_quadruple
    (fst_hash:'a -> int)
    (snd_hash:'b -> int)
    (trd_hash:'c -> int)
    (rth_hash:'d -> int)
    ((a,b,c,d):'a * 'b * 'c * 'd)
  : int =
  (fst_hash a) lxor (snd_hash b) lxor (trd_hash c) lxor (rth_hash d)

let hash_quintuple
    (fst_hash:'a -> int)
    (snd_hash:'b -> int)
    (trd_hash:'c -> int)
    (rth_hash:'d -> int)
    (fth_hash:'e -> int)
    ((a,b,c,d,e):'a * 'b * 'c * 'd * 'e)
  : int =
  (fst_hash a)
  lxor (snd_hash b)
  lxor (trd_hash c)
  lxor (rth_hash d)
  lxor (fth_hash e)
