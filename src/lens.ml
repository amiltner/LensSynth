open Permutation
open Lang

type lens =
  | ConstLens of string * string
  | ConcatLens of lens * lens
  | SwapLens of lens * lens
  | UnionLens of lens * lens
  | ComposeLens of lens * lens
  | IterateLens of lens
  | IdentityLens

type atom_lens =
  | AIterate of dnf_lens
  | AIdentity

and clause_lens = atom_lens list * Permutation.t * string list * string list

and dnf_lens = clause_lens list * Permutation.t
