open Permutation
open Lang
open Regex

type lens =
  | LensConst of string * string
  | LensConcat of lens * lens
  | LensSwap of lens * lens
  | LensUnion of lens * lens
  | LensCompose of lens * lens
  | LensIterate of lens
  | LensIdentity of regex
  | LensInverse of lens
  | LensVariable of id

type atom_lens =
  | AtomLensIterate of dnf_lens
  | AtomLensVariable of lens

and clause_lens = atom_lens list * Permutation.t * string list * string list

and dnf_lens = clause_lens list * Permutation.t

let multiplicative_identity_lens = LensIdentity (multiplicative_identity_regex)

let separate_plus_lens (l:lens) : (lens * lens) option =
  begin match l with
    | LensUnion (l1,l2) -> Some (l1,l2)
    | _ -> None
  end

let separate_times_lens (l:lens) : (lens * lens) option =
  begin match l with
    | LensConcat (l1,l2) -> Some (l1,l2)
    | _ -> None
  end

let create_plus_lens (l1:lens) (l2:lens) : lens =
  LensUnion (l1,l2)

let create_times_lens (l1:lens) (l2:lens) : lens =
  LensConcat (l1,l2)
