open Permutation
open Lang


type atom_lens =
  | AtomLensIterate of dnf_lens
  | AtomLensVariable of lens

and clause_lens = atom_lens list * Permutation.t * string list * string list

and dnf_lens = clause_lens list * Permutation.t

let rec is_sublens (sublens:lens) (suplens:lens) : bool =
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

let rec has_common_sublens (l1:lens) (l2:lens) : bool =
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
