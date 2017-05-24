open Algebra

exception Internal_error of string
val internal_error : string -> string -> in_channel

type id = string
[@@deriving ord, show, hash]

(**** General {{{ *****)

module Regex :
sig
  type t =
    | RegExEmpty
    | RegExBase of string
    | RegExConcat of t * t
    | RegExOr of t * t 
    | RegExStar of t
    | RegExVariable of string
  [@@deriving ord, show, hash]

  val apply_at_every_level : (t -> t) -> t -> t

  val multiplicative_identity : t

  val additive_identity : t

  val separate_plus : t -> (t * t) option

  val separate_times : t -> (t * t) option

  val create_plus : t -> t -> t

  val create_times : t -> t -> t

  val size : t -> int
end
val regex_semiring : (module Semiring with type t = Regex.t)
val regex_star_semiring : (module StarSemiring with type t = Regex.t)

module Lens :
sig
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

  val has_common_sublens : t -> t -> bool

  val multiplicative_identity : t

  val additive_identity : t

  val separate_plus : t -> (t * t) option

  val separate_times : t -> (t * t) option

  val create_plus : t -> t -> t

  val create_times : t -> t -> t

  val size : t -> int
end
val lens_semiring : (module Semiring with type t = Lens.t)
val lens_star_semiring : (module StarSemiring with type t = Lens.t)

type examples = (string * string) list

type specification = (string * Regex.t * Regex.t * (string * string) list)

type declaration =
  | DeclRegexCreation of (id * Regex.t * bool)
  | DeclTestString of (Regex.t * string)
  | DeclSynthesizeLens of specification
  | DeclLensCreation of id * Regex.t * Regex.t * Lens.t
  | DeclTestLens of id * examples

type program = declaration list

type synth_problems = (string * Regex.t * bool) list * (specification list) 
