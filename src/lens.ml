type lens =
  | ConstLens of string * string
  | ConcatLens of lens * lens
  | SwapLens of lens * lens
  | UnionLens of lens * lens
  | IterateLens of lens
  | IdentityLens

type basis_sublens =
  | NLBase of string * string
  | NLStar of unioned_sublens

and concated_sublens = basis_sublens list

and unioned_sublens = concated_sublens list

type normalized_lens =
  | Asdf of string
  | Fdsa of string
