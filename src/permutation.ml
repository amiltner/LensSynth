open Core.Std
open Util

module type Permutation_Sig = sig
  type t

  val create : int list -> t

  val create_all : int -> t list

  val apply : t -> int -> int

  val apply_inverse : t -> int -> int
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
end
