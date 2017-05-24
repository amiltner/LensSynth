open Core
open Util

module type Semiring =
sig
  type t
  val apply_at_every_level : (t -> t) -> t -> t
  val additive_identity : t
  val multiplicative_identity : t
  val separate_plus : t -> (t * t) option
  val separate_times : t -> (t * t) option
  val create_plus : t -> t -> t
  val create_times : t -> t -> t
end

module type StarSemiring =
sig
  type t
  val apply_at_every_level : (t -> t) -> t -> t
  val additive_identity : t
  val multiplicative_identity : t
  val separate_plus : t -> (t * t) option
  val separate_times : t -> (t * t) option
  val separate_star : t -> t option
  val create_plus : t -> t -> t
  val create_times : t -> t -> t
  val create_star : t -> t
end

let maximally_factor_semiring_element
  (type t)
  (module S : Semiring with type t = t)
  : S.t -> S.t =
  let rec separate_into_sum
      (r:S.t)
    : S.t list =
    begin match S.separate_plus r with
      | None -> [r]
      | Some (r1,r2) -> (separate_into_sum r1) @ (separate_into_sum r2)
    end
  in
  let rec separate_into_product
      (r:S.t)
    : S.t list =
    begin match S.separate_times r with
      | None -> [r]
      | Some (r1,r2) -> (separate_into_product r1) @ (separate_into_product r2)
    end
  in
  let combine_nonempty_list_exn
      (combiner:S.t -> S.t -> S.t)
      (rl:S.t list)
    : S.t =
    let (rlf,rll) = split_by_last_exn rl in
    List.fold_right
      ~f:(fun r acc ->
          combiner r acc)
      ~init:rll
      rlf
  in
  let combine_list
      (combiner:S.t -> S.t -> S.t)
      (rl:S.t list)
    : S.t option =
    begin match rl with
      | [] -> None
      | _ -> Some (combine_nonempty_list_exn combiner rl)
    end
  in
  let maximally_factor_current_level
      (product_splitter:S.t list -> (S.t * S.t list))
      (product_combiner:S.t -> S.t -> S.t)
      (he:S.t)
    : S.t =
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
                    begin match combine_list S.create_times pl with
                      | None -> S.multiplicative_identity
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
            let factored_side = combine_nonempty_list_exn S.create_plus al in
            if factored_side = S.multiplicative_identity then
              k
            else
              product_combiner
                k
                factored_side)
        keyed_sum_list
    in
    combine_nonempty_list_exn S.create_plus ringed_list
  in
  Fn.compose
    (fold_until_fixpoint
       (S.apply_at_every_level
          (maximally_factor_current_level
             (Fn.compose swap_double split_by_last_exn)
             (Fn.flip S.create_times))))
    (fold_until_fixpoint
       (S.apply_at_every_level
          (maximally_factor_current_level
             split_by_first_exn
             S.create_times)))

