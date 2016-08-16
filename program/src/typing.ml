open Lang
open Lenscontext


let rec type_lens (lc:LensContext.t) (l:lens) : regex * regex =
  begin match l with
    | LensConst(s1,s2) -> (RegExBase s1, RegExBase s2)
    | LensConcat(l1,l2) ->
      let (r1,s1) = type_lens lc l1 in
      let (r2,s2) = type_lens lc l2 in
      (RegExConcat (r1,r2), RegExConcat (s1,s2))
    | LensSwap(l1,l2) ->
      let (r1,s1) = type_lens lc l1 in
      let (r2,s2) = type_lens lc l2 in
      (RegExConcat (r1,r2), RegExConcat (s2,s1))
    | LensUnion(l1,l2) ->
      let (r1,s1) = type_lens lc l1 in
      let (r2,s2) = type_lens lc l2 in
      (RegExOr (r1,r2), RegExOr (s1,s2))
    | LensCompose(l1,l2) ->
      let (_,s2) = type_lens lc l1 in
      let (r1,_) = type_lens lc l2 in
      (* TODO, check r2 = s1 *)
      (r1,s2)
    | LensIterate (l') ->
      let (r',s') = type_lens lc l' in
      (RegExStar r', RegExStar s')
    | LensIdentity r ->
      (r,r)
    | LensInverse l' ->
      let (r,s) = type_lens lc l' in
      (s,r)
    | LensVariable n ->
      LensContext.lookup_type_exn lc n
  end
