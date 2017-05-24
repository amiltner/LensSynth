open Core
open Util
open Algebra
open Printf


(**** General {{{ *****)
exception Internal_error of string
let internal_error f s = raise @@ Internal_error (sprintf "(%s) %s" f s)

type id = Id of string
[@@deriving ord, show, hash]

let get_string_of_id
    (Id v:id)
  : string =
  v
(***** }}} *****)


(**** Regex {{{ *****)

module Regex =
struct
  type t =
    | RegExEmpty
    | RegExBase of string
    | RegExConcat of t * t
    | RegExOr of t * t 
    | RegExStar of t
    | RegExVariable of id
  [@@deriving ord, show, hash, map]

  let multiplicative_identity = RegExBase ""

  let additive_identity = RegExEmpty

  let separate_plus
      (r:t)
    : (t * t) option =
    begin match r with
      | RegExOr (r1,r2) -> Some (r1,r2)
      | _ -> None
    end

  let separate_times
      (r:t)
    : (t * t) option =
    begin match r with
      | RegExConcat (r1,r2) -> Some (r1,r2)
      | _ -> None
    end

  let separate_star
      (r:t)
    : t option =
    begin match r with
      | RegExStar r' -> Some r'
      | _ -> None
    end

  let separate_userdef
      (r:t)
    : id option =
    begin match r with
      | RegExVariable v -> Some v
      | _ -> None
    end

  let create_plus
      (r1:t)
      (r2:t)
    : t =
    RegExOr (r1,r2)

  let create_times
      (r1:t)
      (r2:t)
    : t =
    RegExConcat (r1,r2)

  let create_star
      (r:t)
    : t =
    RegExStar r

  let create_userdef
      (v:id)
    : t =
    RegExVariable v

  let fold
      ~empty_f:(empty_f:'a)
      ~base_f:(base_f:string -> 'a)
      ~concat_f:(concat_f:'a -> 'a -> 'a)
      ~or_f:(or_f:'a -> 'a -> 'a)
      ~star_f:(star_f:'a -> 'a)
      ~var_f:(var_f:id -> 'a)
    : t -> 'a =
    let rec fold_internal
        (r:t)
      : 'a =
      begin match r with
        | RegExEmpty -> empty_f
        | RegExBase s -> base_f s
        | RegExConcat (r1,r2) ->
          concat_f (fold_internal r1) (fold_internal r2)
        | RegExOr (r1,r2) ->
          or_f (fold_internal r1) (fold_internal r2)
        | RegExStar r' ->
          star_f (fold_internal r')
        | RegExVariable v ->
          var_f v
      end
    in
    fold_internal

  let rec apply_at_every_level
      (f:t -> t)
      (r:t)
    : t =
    fold
      ~empty_f:(f RegExEmpty)
      ~base_f:(fun s -> f (RegExBase s))
      ~concat_f:(fun r1 r2 -> f (RegExConcat (r1,r2)))
      ~or_f:(fun r1 r2 -> f (RegExOr (r1,r2)))
      ~star_f:(fun r' -> f (RegExStar r'))
      ~var_f:(fun v -> f (RegExVariable v))
      r

  let rec applies_for_every_applicable_level
      (f:t -> t option)
    : t -> t list =
    snd
    %
    fold
      ~empty_f:(
        let empty_r = RegExEmpty in
        let level_contribution = option_to_empty_or_singleton (f empty_r) in
        (empty_r, level_contribution))
      ~base_f:(fun s ->
          let base_r = RegExBase s in
          let level_contribution = option_to_empty_or_singleton (f base_r) in
          (base_r, level_contribution))
      ~concat_f:(fun (r1,r1s) (r2,r2s) ->
          let concat_r = RegExConcat (r1,r2) in
          let level_contribution = option_to_empty_or_singleton (f concat_r) in
          let recursed_lefts =
            List.map
              ~f:(fun r1' -> RegExConcat (r1',r2))
              r1s
          in
          let recursed_rights =
            List.map
              ~f:(fun r2' -> RegExConcat (r1,r2'))
              r2s
          in
          (concat_r, level_contribution@recursed_lefts@recursed_rights))
      ~or_f:(fun (r1,r1s) (r2,r2s) ->
          let or_r = RegExOr (r1,r2) in
          let level_contribution = option_to_empty_or_singleton (f or_r) in
          let recursed_lefts =
            List.map
              ~f:(fun r1' -> RegExOr (r1',r2))
              r1s
          in
          let recursed_rights =
            List.map
              ~f:(fun r2' -> RegExOr (r1,r2'))
              r2s
          in
          (or_r, level_contribution@recursed_lefts@recursed_rights))
      ~star_f:(fun (r',r's) ->
          let star_r = RegExStar r' in
          let level_contribution = option_to_empty_or_singleton (f star_r) in
          let recursed_inner =
            List.map
              ~f:(fun r'' -> RegExStar r'')
              r's
          in
          (star_r, level_contribution@recursed_inner))
      ~var_f:(fun v ->
          let var_r = RegExVariable v in
          let level_contribution = option_to_empty_or_singleton (f var_r) in
          (var_r, level_contribution))

  let rec size
    : t -> int =
    fold
      ~empty_f:1
      ~base_f:(fun _ -> 1)
      ~concat_f:(fun n1 n2 -> 1+n1+n2)
      ~or_f:(fun n1 n2 -> 1+n1+n2)
      ~star_f:(fun n -> 1+n)
      ~var_f:(fun _ -> 1)
end

let regex_semiring = (module Regex : Semiring with type t = Regex.t)
let regex_star_semiring = (module Regex : StarSemiring with type t = Regex.t)
(***** }}} *****)




(**** Lens {{{ *****)

module Lens =
struct
  type t =
    | LensConst of string * string
    | LensConcat of t * t
    | LensSwap of t * t
    | LensUnion of t * t
    | LensCompose of t * t
    | LensIterate of t
    | LensIdentity of Regex.t
    | LensInverse of t
    | LensVariable of id
    | LensPermute of (int list) (*Permutation.t*) * (t list)
  [@@deriving ord, show, hash]


  let multiplicative_identity = LensIdentity (Regex.multiplicative_identity)

  let additive_identity = LensIdentity (Regex.additive_identity)

  let separate_plus (l:t) : (t * t) option =
    begin match l with
      | LensUnion (l1,l2) -> Some (l1,l2)
      | _ -> None
    end

  let separate_times (l:t) : (t * t) option =
    begin match l with
      | LensConcat (l1,l2) -> Some (l1,l2)
      | _ -> None
    end

  let separate_star (l:t) : t option =
    begin match l with
      | LensIterate l' -> Some l'
      | _ -> None
    end

  let create_plus (l1:t) (l2:t) : t =
    LensUnion (l1,l2)

  let create_times (l1:t) (l2:t) : t =
    LensConcat (l1,l2)

  let create_star (l:t) : t =
    LensIterate l

  let rec size (l:t) : int =
    begin match l with
      | LensConst _ -> 1
      | LensConcat (l1,l2) ->
        1 + (size l1) + (size l2)
      | LensCompose (l1,l2) ->
        1 + (size l1) + (size l2)
      | LensSwap (l1,l2) ->
        1 + (size l1) + (size l2)
      | LensUnion (l1,l2) ->
        1 + (size l1) + (size l2)
      | LensIterate (l') ->
        1 + (size l')
      | LensIdentity _ -> 1
      | LensInverse l' ->
        1 + (size l')
      | LensVariable _ -> 1
      | LensPermute (_,ls) ->
        1 + (List.fold_left
               ~f:(fun acc l' -> acc + (size l'))
               ~init:0
               ls)
    end

  let rec is_sublens (sublens:t) (suplens:t) : bool =
    if sublens = suplens then
      true
    else
      begin match suplens with
        | LensConcat (l1,l2) ->
          (is_sublens sublens l1) || (is_sublens sublens l2)
        | LensSwap (l1,l2) ->
          (is_sublens sublens l1) || (is_sublens sublens l2)
        | LensUnion (l1,l2) ->
          (is_sublens sublens l1) || (is_sublens sublens l2)
        | LensCompose (l1,l2) ->
          (is_sublens sublens l1) || (is_sublens sublens l2)
        | LensIterate l' ->
          is_sublens sublens l'
        | LensInverse l' ->
          is_sublens sublens l'
        | _ -> false
      end

  let rec has_common_sublens (l1:t) (l2:t) : bool =
    begin match l1 with
      | LensConcat (l11,l12) ->
        (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
      | LensSwap (l11,l12) ->
        (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
      | LensUnion (l11,l12) ->
        (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
      | LensCompose (l11,l12) ->
        (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
      | LensIterate l1' ->
        has_common_sublens l1' l2
      | LensInverse l1' ->
        has_common_sublens l1' l2
      | _ -> is_sublens l1 l2
    end

  let rec apply_at_every_level (f:t -> t) (l:t) : t =
    let l =
      begin match l with
        | LensConcat (l1,l2) ->
          LensConcat (apply_at_every_level f l1, apply_at_every_level f l2)
        | LensSwap (l1,l2) ->
          LensSwap (apply_at_every_level f l1, apply_at_every_level f l2)
        | LensUnion (l1,l2) ->
          LensUnion (apply_at_every_level f l1, apply_at_every_level f l2)
        | LensCompose (l1,l2) ->
          LensCompose (apply_at_every_level f l1, apply_at_every_level f l2)
        | LensIterate (l') ->
          LensIterate (apply_at_every_level f l')
        | LensInverse (l') ->
          LensInverse (apply_at_every_level f l')
        | _ -> l
      end
    in
    f l

  let rec applies_for_every_applicable_level
      (_:t -> t option)
      (_:t)
    : t list =
    failwith "TODO"
end

let lens_semiring = (module Lens : Semiring with type t = Lens.t)
let lens_star_semiring = (module Lens : StarSemiring with type t = Lens.t)

(***** }}} *****)



(**** Language {{{ *****)

type examples = (string * string) list

type specification = (id * Regex.t * Regex.t * (string * string) list)

type declaration =
  | DeclRegexCreation of (id * Regex.t * bool)
  | DeclTestString of (Regex.t * string)
  | DeclSynthesizeLens of specification
  | DeclLensCreation of id * Regex.t * Regex.t * Lens.t
  | DeclTestLens of id * examples

type program = declaration list

type synth_problems = (id * Regex.t * bool) list * (specification list) 

(***** }}} *****)


