open Core.Std
open Util

module type Permutation_Sig = sig
  type t

  val create : int list -> t

  val create_from_doubles : (int * int) list -> t

  val create_from_constraints : int -> (int * int) list
                                    -> (int * int) list -> (t * ((int * int) list)) option

  val create_all : int -> t list

  val apply : t -> int -> int

  val apply_inverse : t -> int -> int
  
  val apply_to_list_exn : t -> 'a list -> 'a list

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

  let rec create_from_doubles (mapping:(int*int) list) : t =
    let len = List.length mapping in
    let (mapping_ls,mapping_rs) = List.unzip mapping in
    let contains_dup_l = List.contains_dup
        ~compare:(fun x y -> x - y)
        mapping_ls in
    let contains_dup_r = List.contains_dup
        ~compare:(fun x y -> x - y)
        mapping_rs in
    let out_of_range = List.exists
        ~f:(fun (x,y) -> x >= len || x < 0 || y >= len || y < 0)
        mapping in
    if contains_dup_l || contains_dup_r || out_of_range then
      failwith "Not Bijection"
    else
      let sorted_by_first = List.sort
        ~cmp:(fun (x,_) (y,_) -> x - y)
        mapping in
      List.map ~f:(fun (x,y) -> y) sorted_by_first

  let create_from_constraints (len:int) (invalid_parts:(int*int) list)
                              (required_parts:(int*int) list)
                              : (t * ((int*int) list)) option =

    let rec create_from_constraints_internal (len:int)
                                             (invalid_parts:(int*int) list)
                                             (required_parts:(int*int) list)
                                             (unused_partsl:int list)
                                             (unused_partsr:int list)
                                             (guessed_parts:(int*int) list)
                                             (continuation:
                                               ((t * ((int*int) list)) option)
                                            -> ((t * ((int*int) list)) option))
                                             (unused_l:int)
                                           : (t * ((int*int) list)) option =
      begin match unused_partsl with
      | [] -> Some (create_from_doubles required_parts, guessed_parts)
      | hl::tl ->
          let choice = split_by_first_satisfying
            (fun x -> not (List.mem invalid_parts (hl,x)))
            unused_partsr in
          begin match choice with
          | None -> continuation None
          | Some (hr,tr) ->
              let ctn = (fun potential_solution ->
                begin match potential_solution with
              | None ->
                  create_from_constraints_internal
                          len
                          ((hl,hr)::invalid_parts)
                          required_parts
                          unused_partsl
                          unused_partsr
                          guessed_parts
                          continuation
                          unused_l
              | Some _ -> continuation(potential_solution)
              end) in
              create_from_constraints_internal
                len
                invalid_parts
                ((hl,hr)::required_parts)
                tl
                tr
                ((hl,hr)::guessed_parts)
                ctn
                (unused_l-1)
          end
      end in
  if (List.exists
        ~f:(fun invalid_part -> List.mem required_parts invalid_part)
        invalid_parts) then
    None
  else
    let available_parts = range 0 (len - 1) in
    let (used_partsl,used_partsr) = List.unzip required_parts in
    let (unused_partsl,unused_partsr) = List.fold_left
      ~f:(fun (l,r) x ->
            let unused_in_left = not (List.mem used_partsl x) in
            let unused_in_right = not (List.mem used_partsr x) in
            let l' = if unused_in_left then x::l else l in
            let r' = if unused_in_right then x::r else r in
            (l',r'))
      ~init:([],[])
      available_parts in
    create_from_constraints_internal
      len
      invalid_parts
      required_parts
      unused_partsl
      unused_partsr
      []
      (fun x -> x)
      (List.length unused_partsl)

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

  let apply_to_list_exn (permutation:t) (l:'a list) : 'a list =
    let permutation_list_combo = List.zip_exn permutation l in
    let sorted_by_perm = List.sort
      ~cmp:(fun (p1,x1) (p2,x2) -> p1 - p2)
      permutation_list_combo in
    let (_,l') = List.unzip sorted_by_perm in
    l'

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
