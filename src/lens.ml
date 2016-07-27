open Permutation
open Lang

type lens =
  | ConstLens of string * string
  | ConcatLens of lens * lens
  | SwapLens of lens * lens
  | UnionLens of lens * lens
  | ComposeLens of lens * lens
  | IterateLens of lens
  | IdentityLens of regex
  | InverseLens of lens

type atom_lens =
  | AIterate of dnf_lens
  | AIdentity of string

and clause_lens = atom_lens list * Permutation.t * string list * string list

and dnf_lens = clause_lens list * Permutation.t

let rec is_sublens (sublens:lens) (suplens:lens) : bool =
  if sublens = suplens then
    true
  else
    begin match suplens with
      | ConcatLens (l1,l2) ->
        (is_sublens sublens l1) || (is_sublens sublens l2)
      | SwapLens (l1,l2) ->
        (is_sublens sublens l1) || (is_sublens sublens l2)
      | UnionLens (l1,l2) ->
        (is_sublens sublens l1) || (is_sublens sublens l2)
      | ComposeLens (l1,l2) ->
        (is_sublens sublens l1) || (is_sublens sublens l2)
      | IterateLens l' ->
        is_sublens sublens l'
      | InverseLens l' ->
        is_sublens sublens l'
      | _ -> false
    end

let rec has_common_sublens (l1:lens) (l2:lens) : bool =
  begin match l1 with
    | ConcatLens (l11,l12) ->
      (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
    | SwapLens (l11,l12) ->
      (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
    | UnionLens (l11,l12) ->
      (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
    | ComposeLens (l11,l12) ->
      (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
    | IterateLens l1' ->
      has_common_sublens l1' l2
    | InverseLens l1' ->
      has_common_sublens l1' l2
    | _ -> is_sublens l1 l2
    end

let rec type_lens (l:lens) : regex * regex =
  begin match l with
    | ConstLens(s1,s2) -> (RegExBase s1, RegExBase s2)
    | ConcatLens(l1,l2) ->
      let (r1,s1) = type_lens l1 in
      let (r2,s2) = type_lens l2 in
      (RegExConcat (r1,r2), RegExConcat (s1,s2))
    | SwapLens(l1,l2) ->
      let (r1,s1) = type_lens l1 in
      let (r2,s2) = type_lens l2 in
      (RegExConcat (r1,r2), RegExConcat (s2,s1))
    | UnionLens(l1,l2) ->
      let (r1,s1) = type_lens l1 in
      let (r2,s2) = type_lens l2 in
      (RegExOr (r1,r2), RegExOr (s1,s2))
    | ComposeLens(l1,l2) ->
      let (_,s2) = type_lens l1 in
      let (r1,_) = type_lens l2 in
      (* TODO, check r2 = s1 *)
      (r1,s2)
    | IterateLens (l') ->
      let (r',s') = type_lens l' in
      (RegExStar r', RegExStar s')
    | IdentityLens r ->
      (r,r)
    | InverseLens l' ->
      let (r,s) = type_lens l' in
      (s,r)
  end
