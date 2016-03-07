open Permutation

type lens =
  | ConstLens of string * string
  | ConcatLens of lens * lens
  | SwapLens of lens * lens
  | UnionLens of lens * lens
  | IterateLens of lens
  | IdentityLens

type basis_sublens =
  | NLIdentityLens
  | NLStar of unioned_sublens

and concated_sublens = (basis_sublens list * Permutation.t *
  (string option) list * (string option) list)

and unioned_sublens = concated_sublens list * Permutation.t

type normalized_lens = unioned_sublens
