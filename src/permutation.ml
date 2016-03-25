open Core.Std
open Util

module type Permutation_Sig = sig
  type t

  val create : int list -> t

  val create_all : int -> t list

  val apply : t -> int -> int

  val apply_inverse : t -> int -> int
  
  val apply_to_list : t -> 'a list -> 'a list

  val apply_inverse_to_list : t -> 'a list -> 'a list

  val pp : t -> string
end

module Permutation : Permutation_Sig = struct
  type t = int list

  let rec create (mapping:int list) =
    let len = List.length mapping in
    List.rev
      (List.fold_left
      ~f:(fun acc x ->
        if ((List.mem acc x) || (x >= len) || (x < 0)) then
          failwith "Not Bijection"
        else
          x::acc)
      ~init:[]
      mapping)

  let rm x l = List.filter ~f:((<>) x) l  

  let rec create_all (n:int) : t list =
    let rec permutations = function  
    | [] -> []
    | x::[] -> [[x]]
    | l -> List.fold_left ~f:(fun acc x -> acc @ List.map ~f:(fun p -> x::p)
             (permutations (rm x l))) ~init:[] l
    in
    permutations (range 0 (n-1))

  let apply (permutation:t) (n:int) =
    begin match (List.nth permutation n) with
    | None -> failwith "out of range"
    | Some i -> i
    end

  let apply_inverse (permutation:t) (n:int) =
    let rec find x lst =
      begin match lst with
      | [] -> failwith "out of range"
      | h::t -> if x = h then 0 else 1 + find x t
      end in
    find n permutation

  let apply_to_list (permutation:t) (l:'a list) : 'a list =
    List.map
    ~f:(fun v -> List.nth_exn l v)
    permutation

  let apply_inverse_to_list (permutation:t) (l:'a list) : 'a list =
    let indexed_permutation = List.mapi
      ~f:(fun i x -> (i,x))
      permutation in
    List.map
      ~f:(fun v ->
        let (index,_) = List.find_exn
          ~f:(fun (i,x) -> x = v)
          indexed_permutation in
        List.nth_exn l index
      )
      (range 0 ((List.length l)-1))

  let pp (permutation:t) : string =
    String.concat
      (List.mapi
        ~f:(fun x y -> (string_of_int x) ^ "->" ^ (string_of_int y))
        permutation)
      ~sep: " , "
    
end
