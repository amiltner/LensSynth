open Core
open Util
open Lang
open Regexcontext

let rec make_regex_safe_in_smaller_context
    (rc_smaller:RegexContext.t)
    (rc_larger:RegexContext.t)
    (r:Regex.t)
  : Regex.t =
  begin match r with
    | Regex.RegExEmpty -> Regex.RegExEmpty
    | Regex.RegExBase _ -> r
    | Regex.RegExConcat (r1,r2) ->
      let r1 = make_regex_safe_in_smaller_context rc_smaller rc_larger r1 in
      let r2 = make_regex_safe_in_smaller_context rc_smaller rc_larger r2 in
      Regex.RegExConcat (r1,r2)
    | Regex.RegExOr (r1,r2) ->
      let r1 = make_regex_safe_in_smaller_context rc_smaller rc_larger r1 in
      let r2 = make_regex_safe_in_smaller_context rc_smaller rc_larger r2 in
      Regex.RegExOr (r1,r2)
    | Regex.RegExStar r' ->
      let r' = make_regex_safe_in_smaller_context rc_smaller rc_larger r' in
      Regex.RegExStar r'
    | Regex.RegExVariable rn ->
      begin match (RegexContext.lookup rc_smaller rn) with
        | None ->
          let r = RegexContext.lookup_exn rc_larger rn in
          make_regex_safe_in_smaller_context rc_smaller rc_larger r
        | Some _ -> r
      end
  end

let simplify_regex : Regex.t -> Regex.t =
  let maximally_factor_regex : Regex.t -> Regex.t =
    Algebra.maximally_factor_semiring_element
      regex_semiring
  in
  let rec clean_regex (r:Regex.t) : Regex.t =
    begin match r with
      | Regex.RegExConcat(x,y) ->
        let x = clean_regex x in
        let y = clean_regex y in
        begin match (x,y) with
          | (Regex.RegExBase "",_) -> y
          | (_,Regex.RegExBase "") -> x
          | (Regex.RegExEmpty,_) -> Regex.RegExEmpty
          | (_,Regex.RegExEmpty) -> Regex.RegExEmpty
          | _ -> Regex.RegExConcat(x,y)
        end
      | Regex.RegExOr(x,y) ->
        let x = clean_regex x in
        let y = clean_regex y in
        begin match (x,y) with
          | (Regex.RegExEmpty,_) -> y
          | (_,Regex.RegExEmpty) -> x
          | _ -> Regex.RegExOr(x,y)
        end
      | Regex.RegExStar(x) ->
        let x = clean_regex x in
        begin match x with
          | Regex.RegExEmpty -> Regex.RegExBase ""
          | _ -> Regex.RegExStar x
        end
      | _ -> r
    end
  in

  let merge_concated_bases : Regex.t -> Regex.t =
    let rec retrieve_rightmost_concated_base
        (r:Regex.t)
      : (Regex.t option * string option) =
      begin match r with
        | Regex.RegExConcat (r1,r2) ->
          begin match retrieve_rightmost_concated_base r2 with
            | (None, so) -> (Some r1, so)
            | (Some r2, so) -> (Some (Regex.RegExConcat (r1,r2)),so)
          end
        | Regex.RegExBase s -> (None, Some s)
        | _ -> (Some r, None)
      end
    in
    let rec try_insert_into_leftmost_concated_base
        (r:Regex.t)
        (s1:string)
      : Regex.t option =
      begin match r with
        | Regex.RegExConcat (r1,r2) ->
          Option.map
            ~f:(fun r1 -> Regex.RegExConcat (r1,r2))
            (try_insert_into_leftmost_concated_base r1 s1)
        | Regex.RegExBase s2 ->
          Some (Regex.RegExBase (s1^s2))
        | _ -> None
      end
    in
    let merge_concated_bases_current_level
        (r:Regex.t)
      : Regex.t =
      begin match r with
        | Regex.RegExConcat (r1,r2) ->
          begin match retrieve_rightmost_concated_base r1 with
            | (r1o,Some s1) ->
              begin match try_insert_into_leftmost_concated_base r2 s1 with
                | None -> r
                | Some r2 ->
                  begin match r1o with
                    | None -> r2
                    | Some r1 -> Regex.RegExConcat (r1,r2)
                  end
              end
            | (_, None) -> r
          end
        | _ -> r
      end
    in

    Regex.apply_at_every_level merge_concated_bases_current_level
  in

  fold_until_fixpoint
    (merge_concated_bases
     % clean_regex
     % maximally_factor_regex)
