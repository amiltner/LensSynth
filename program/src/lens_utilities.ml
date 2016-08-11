open Core.Std
open Util
open Lens
open Regex
open Regexcontext
open Regex_utilities

let rec apply_at_every_level_lens (f:lens -> lens) (l:lens) : lens =
  let l =
    begin match l with
      | LensConcat (l1,l2) ->
        LensConcat (apply_at_every_level_lens f l1, apply_at_every_level_lens f l2)
      | LensSwap (l1,l2) ->
        LensSwap (apply_at_every_level_lens f l1, apply_at_every_level_lens f l2)
      | LensUnion (l1,l2) ->
        LensUnion (apply_at_every_level_lens f l1, apply_at_every_level_lens f l2)
      | LensCompose (l1,l2) ->
        LensCompose (apply_at_every_level_lens f l1, apply_at_every_level_lens f l2)
      | LensIterate (l') ->
        LensIterate (apply_at_every_level_lens f l')
      | LensInverse (l') ->
        LensInverse (apply_at_every_level_lens f l')
      | _ -> l
    end
  in
  f l

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

let rec make_lens_safe_in_smaller_context
    (rc_smaller:RegexContext.t)
    (rc_larger:RegexContext.t)
    (l:lens)
  : lens =
  begin match l with
    | LensConst _ -> l
    | LensConcat (l1,l2) ->
      let l1 = make_lens_safe_in_smaller_context rc_smaller rc_larger l1 in
      let l2 = make_lens_safe_in_smaller_context rc_smaller rc_larger l2 in
      LensConcat (l1,l2)
    | LensSwap (l1,l2) ->
      let l1 = make_lens_safe_in_smaller_context rc_smaller rc_larger l1 in
      let l2 = make_lens_safe_in_smaller_context rc_smaller rc_larger l2 in
      LensSwap (l1,l2)
    | LensUnion (l1,l2) ->
      let l1 = make_lens_safe_in_smaller_context rc_smaller rc_larger l1 in
      let l2 = make_lens_safe_in_smaller_context rc_smaller rc_larger l2 in
      LensUnion (l1,l2)
    | LensCompose (l1,l2) ->
      let l1 = make_lens_safe_in_smaller_context rc_smaller rc_larger l1 in
      let l2 = make_lens_safe_in_smaller_context rc_smaller rc_larger l2 in
      LensCompose (l1,l2)
    | LensIterate l' ->
      let l' = make_lens_safe_in_smaller_context rc_smaller rc_larger l' in
      LensIterate l'
    | LensIdentity r ->
      LensIdentity (make_regex_safe_in_smaller_context rc_smaller rc_larger r)
    | LensInverse l' ->
      let l' = make_lens_safe_in_smaller_context rc_smaller rc_larger l' in
      LensInverse l'
    | LensVariable _ -> l
  end


let simplify_lens : lens -> lens =
  let maximally_factor_lens : lens -> lens =
    maximally_factor_hemiring_element
      apply_at_every_level_lens
      multiplicative_identity_lens
      separate_plus_lens
      separate_times_lens
      create_plus_lens
      create_times_lens
  in
  let distribute_identities (l:lens) : lens =
    let merge_concated_identities : lens -> lens =
      let rec retrieve_rightmost_identity
          (l:lens)
        : (lens option * regex option) =
        begin match l with
          | LensConcat (l1,l2) ->
            begin match retrieve_rightmost_identity l2 with
              | (None, ro) -> (Some l1, ro)
              | (Some l2, ro) -> (Some (LensConcat (l1,l2)),ro)
            end
          | LensIdentity r -> (None, Some r)
          | _ -> (Some l, None)
        end
      in
      let rec try_insert_into_leftmost_identity
          (l:lens)
          (r1:regex)
        : lens option =
        begin match l with
          | LensConcat (l1,l2) ->
            Option.map
              ~f:(fun l1 -> LensConcat (l1,l2))
              (try_insert_into_leftmost_identity l1 r1)
          | LensIdentity r2 ->
            Some (LensIdentity (RegExConcat (r1,r2)))
          | _ -> None
        end
      in
      let merge_concated_identities_current_level
          (l:lens)
        : lens =
        begin match l with
          | LensConcat (l1,l2) ->
            begin match retrieve_rightmost_identity l1 with
              | (l1o,Some r1) ->
                begin match try_insert_into_leftmost_identity l2 r1 with
                  | None -> l
                  | Some l2 ->
                    begin match l1o with
                      | None -> l2
                      | Some l1 -> LensConcat (l1,l2)
                    end
                end
              | (_, None) -> l
            end
          | _ -> l
        end
      in
      apply_at_every_level_lens merge_concated_identities_current_level
    in

    let merge_ored_identities
        (l:lens)
      : lens =
      let or_lens_with_identity
          (lo:lens option)
          (ro:regex option)
        : lens =
        begin match (lo,ro) with
          | (None,None) -> failwith "badly implemented merge"
          | (None, Some r) -> LensIdentity r
          | (Some l, None) -> l
          | (Some l, Some r) ->
            LensUnion (l, LensIdentity r)
        end
      in
      let or_regex_options
          (r1o:regex option)
          (r2o:regex option)
        : regex option =
        begin match (r1o,r2o) with
          | (None, None) -> None
          | (Some r1, None) -> Some r1
          | (None, Some r2) -> Some r2
          | (Some r1, Some r2) -> Some (RegExOr (r1,r2))
        end
      in
      let or_lens_options
          (l1o:lens option)
          (l2o:lens option)
        : lens option =
        begin match (l1o,l2o) with
          | (None, None) -> None
          | (Some l1, None) -> Some l1
          | (None, Some l2) -> Some l2
          | (Some l1, Some l2) -> Some (LensUnion (l1,l2))
        end
      in
      let rec merge_ored_identities_internal
          (l:lens)
        : ((lens option) * (regex option)) =
        begin match l with
          | LensConst _ -> (Some l, None)
          | LensConcat (l1,l2) ->
            let (l1o,r1o) = merge_ored_identities_internal l1 in
            let (l2o,r2o) = merge_ored_identities_internal l2 in
            let l1 = or_lens_with_identity l1o r1o in
            let l2 = or_lens_with_identity l2o r2o in
            (Some (LensConcat (l1,l2)), None)
          | LensSwap (l1,l2) ->
            let (l1o,r1o) = merge_ored_identities_internal l1 in
            let (l2o,r2o) = merge_ored_identities_internal l2 in
            let l1 = or_lens_with_identity l1o r1o in
            let l2 = or_lens_with_identity l2o r2o in
            (Some (LensSwap (l1,l2)), None)
          | LensUnion (l1,l2) ->
            let (l1o,r1o) = merge_ored_identities_internal l1 in
            let (l2o,r2o) = merge_ored_identities_internal l2 in
            (or_lens_options l1o l2o, or_regex_options r1o r2o)
          | LensCompose (l1,l2) ->
            let (l1o,r1o) = merge_ored_identities_internal l1 in
            let (l2o,r2o) = merge_ored_identities_internal l2 in
            let l1 = or_lens_with_identity l1o r1o in
            let l2 = or_lens_with_identity l2o r2o in
            (Some (LensCompose (l1,l2)), None)
          | LensIterate l' ->
            let (lo',ro') = merge_ored_identities_internal l' in
            let l' = or_lens_with_identity lo' ro' in
            (Some (LensIterate l'), None)
          | LensIdentity r ->
            (None, if r = RegExEmpty then None else Some r)
          | LensInverse l' ->
            let (lo',ro') = merge_ored_identities_internal l' in
            let l' = or_lens_with_identity lo' ro' in
            (Some (LensInverse l'), None)
          | LensVariable _ -> (Some l, None)
        end
      in
      let (lo,ro) = merge_ored_identities_internal l in
      or_lens_with_identity lo ro
    in

    let distribute_iteration : lens -> lens =
      let distribute_iteration_single_level (l:lens) : lens =
        begin match l with
          | LensIterate (LensIdentity r) -> LensIdentity (RegExStar r)
          | _ -> l
        end
      in
      apply_at_every_level_lens distribute_iteration_single_level
    in


    l
    |> merge_ored_identities
    |> merge_concated_identities
    |> distribute_iteration
  in

  let clean_identities : lens -> lens =
    let clean_identities_single_level (l:lens) : lens =
      begin match l with
        | LensIdentity r -> LensIdentity (simplify_regex r)
        | _ -> l
      end
    in
    apply_at_every_level_lens clean_identities_single_level
  in

  let distribute_inverses : lens -> lens =
    let distribute_inverses_current_level (l:lens) : lens =
      begin match l with
        | LensInverse l' ->
          begin match l' with
            | LensConst (s1,s2) ->
              LensConst(s2,s1)
            | LensConcat (l1',l2') ->
              LensConcat (LensInverse l1', LensInverse l2')
            | LensSwap (l1',l2') ->
              LensSwap (LensInverse l2', LensInverse l1')
            | LensUnion (l1',l2') ->
              LensUnion (LensInverse l1', LensInverse l2')
            | LensCompose (l1',l2') ->
              LensCompose (LensInverse l2', LensInverse l1')
            | LensIterate l'' ->
              LensIterate (LensInverse l'')
            | LensIdentity r ->
              LensIdentity r
            | LensInverse l'' ->
              l''
            | LensVariable _ ->
              l'
          end
        | _ -> l
      end
    in
    apply_at_every_level_lens distribute_inverses_current_level
  in

  let identify_identity_consts : lens -> lens =
    let identify_identity_consts_current_level (l:lens) : lens =
      begin match l with
        | LensConst(s1,s2) ->
          if s1 = s2 then
            LensIdentity (RegExBase s1)
          else
            l
        | _ -> l
      end
    in
    apply_at_every_level_lens identify_identity_consts_current_level
  in

  let remove_identity_identities : lens -> lens =
    let remove_identity_identities_current_level (l:lens) : lens =
      begin match l with
        | LensConcat(LensIdentity (RegExBase ""), l) -> l
        | LensConcat(l, LensIdentity (RegExBase "")) -> l
        | LensSwap  (LensIdentity (RegExBase ""), l) -> l
        | LensSwap  (l, LensIdentity (RegExBase "")) -> l
        | LensUnion (l, LensIdentity (RegExEmpty))   -> l
        | LensUnion (LensIdentity (RegExEmpty), l)   -> l
        | _ -> l
      end
    in
    apply_at_every_level_lens remove_identity_identities_current_level
  in

  let split_consts_into_concats_leftfirst : lens -> lens =
    let split_consts_into_concats_leftfirst_current_level (l:lens) : lens =
      begin match l with
        | LensConst(s1,s2) -> LensConcat(LensConst(s1,""),LensConst("",s2))
        | _ -> l
      end
    in
    apply_at_every_level_lens split_consts_into_concats_leftfirst_current_level
  in

  let split_consts_into_concats_rightfirst : lens -> lens =
    let split_consts_into_concats_rightfirst_current_level (l:lens) : lens =
      begin match l with
        | LensConst(s1,s2) -> LensConcat(LensConst(s1,""),LensConst("",s2))
        | _ -> l
      end
    in
    apply_at_every_level_lens split_consts_into_concats_rightfirst_current_level
  in

  let separate_emptystring_consts : lens -> lens =
    let string_to_singlecharstring_list (s:string) : string list =
      let cl = string_to_char_list s in
      List.map ~f:Char.to_string cl
    in
    let separate_emptystring_consts_current_level (l:lens) : lens =
      begin match l with
        | LensConst("","") -> l
        | LensConst("",s2) ->
          let sl = string_to_singlecharstring_list s2 in
          let (fs,ls) = split_by_last_exn sl in
          List.fold_right
            ~f:(fun s acc ->
                LensConcat(LensConst("",s),acc))
            ~init:(LensConst("",ls))
            fs
        | LensConst(s1,"") ->
          let sl = string_to_singlecharstring_list s1 in
          let (fs,ls) = split_by_last_exn sl in
          List.fold_right
            ~f:(fun s acc ->
                LensConcat(LensConst(s,""),acc))
            ~init:(LensConst(ls,""))
            fs
        | _ -> l
      end
    in

    apply_at_every_level_lens separate_emptystring_consts_current_level
  in

  let merge_concated_consts : lens -> lens =
    let rec retrieve_rightmost_const
        (l:lens)
      : (lens option * ((string*string) option)) =
      begin match l with
        | LensConcat (l1,l2) ->
          begin match retrieve_rightmost_const l2 with
            | (None, sso) -> (Some l1, sso)
            | (Some l2, sso) -> (Some (LensConcat (l1,l2)),sso)
          end
        | LensConst(s1,s2) -> (None, Some (s1,s2))
        | _ -> (Some l, None)
      end
    in
    let rec try_insert_into_leftmost_const
        (l:lens)
        (s1:string)
        (s2:string)
      : lens option =
      begin match l with
        | LensConcat (l1,l2) ->
          Option.map
            ~f:(fun l1 -> LensConcat (l1,l2))
            (try_insert_into_leftmost_const l1 s1 s2)
        | LensConst (t1,t2) ->
          Some (LensConst (s1^t1,s2^t2))
        | _ -> None
      end
    in
    let merge_concated_consts_current_level
        (l:lens)
      : lens =
      begin match l with
        | LensConcat (l1,l2) ->
          begin match retrieve_rightmost_const l1 with
            | (l1o,Some (s1,s2)) ->
              begin match try_insert_into_leftmost_const l2 s1 s2 with
                | None -> l
                | Some l2 ->
                  begin match l1o with
                    | None -> l2
                    | Some l1 -> LensConcat (l1,l2)
                  end
              end
            | (_, None) -> l
          end
        | _ -> l
      end
    in
    apply_at_every_level_lens merge_concated_consts_current_level
  in

  let perform_cleanups =
    distribute_inverses
    % identify_identity_consts
    % merge_concated_consts
    % remove_identity_identities
    % clean_identities
    % distribute_identities
    % maximally_factor_lens
    % separate_emptystring_consts
  in
  
  fold_until_fixpoint
    (perform_cleanups
     % split_consts_into_concats_rightfirst
     % perform_cleanups
     % split_consts_into_concats_leftfirst)
