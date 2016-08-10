open Regex
open Regexcontext

let rec make_regex_safe_in_smaller_context
    (rc_smaller:RegexContext.t)
    (rc_larger:RegexContext.t)
    (r:regex)
  : regex =
  begin match r with
    | RegExEmpty -> RegExEmpty
    | RegExBase _ -> r
    | RegExConcat (r1,r2) ->
      let r1 = make_regex_safe_in_smaller_context rc_smaller rc_larger r1 in
      let r2 = make_regex_safe_in_smaller_context rc_smaller rc_larger r2 in
      RegExConcat (r1,r2)
    | RegExOr (r1,r2) ->
      let r1 = make_regex_safe_in_smaller_context rc_smaller rc_larger r1 in
      let r2 = make_regex_safe_in_smaller_context rc_smaller rc_larger r2 in
      RegExOr (r1,r2)
    | RegExStar r' ->
      let r' = make_regex_safe_in_smaller_context rc_smaller rc_larger r' in
      RegExStar r'
    | RegExVariable rn ->
      begin match (RegexContext.lookup rc_smaller rn) with
        | None ->
          let r = RegexContext.lookup_exn rc_larger rn in
          make_regex_safe_in_smaller_context rc_smaller rc_larger r
        | Some _ -> r
      end
    | RegExMapped _ -> r
  end
