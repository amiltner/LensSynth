open Core.Std
open Util

type regex =
  | RegExEmpty
  | RegExBase of string
  | RegExConcat of regex * regex
  | RegExOr of regex * regex
  | RegExStar of regex
  | RegExVariable of string
  | RegExMapped of int

let multiplicative_identity_regex = RegExBase ""

let separate_plus_regex (r:regex) : (regex * regex) option =
  begin match r with
    | RegExOr (r1,r2) -> Some (r1,r2)
    | _ -> None
  end

let separate_times_regex (r:regex) : (regex * regex) option =
  begin match r with
    | RegExConcat (r1,r2) -> Some (r1,r2)
    | _ -> None
  end

let create_plus_regex (r1:regex) (r2:regex) : regex =
  RegExOr (r1,r2)

let create_times_regex (r1:regex) (r2:regex) : regex =
  RegExConcat (r1,r2)

let rec apply_at_every_level_regex (f:regex -> regex) (r:regex) : regex =
  let r =
    begin match r with
      | RegExConcat (r1,r2) ->
        RegExConcat (apply_at_every_level_regex f r1, apply_at_every_level_regex f r2)
      | RegExOr (r1,r2) ->
        RegExOr (apply_at_every_level_regex f r1, apply_at_every_level_regex f r2)
      | RegExStar r' ->
        RegExStar (apply_at_every_level_regex f r')
      | _ -> r
    end
  in
  f r






let simplify_regex : regex -> regex =
  let maximally_factor_regex : regex -> regex =
    maximally_factor_hemiring_element
      apply_at_every_level_regex
      multiplicative_identity_regex
      separate_plus_regex
      separate_times_regex
      create_plus_regex
      create_times_regex
  in
  let rec clean_regex (r:regex) : regex =
    begin match r with
      | RegExConcat(x,y) ->
        let x = clean_regex x in
        let y = clean_regex y in
        begin match (x,y) with
          | (RegExBase "",_) -> y
          | (_,RegExBase "") -> x
          | (RegExEmpty,_) -> RegExEmpty
          | (_,RegExEmpty) -> RegExEmpty
          | _ -> RegExConcat(x,y)
        end
      | RegExOr(x,y) ->
        let x = clean_regex x in
        let y = clean_regex y in
        begin match (x,y) with
          | (RegExEmpty,_) -> y
          | (_,RegExEmpty) -> x
          | _ -> RegExOr(x,y)
        end
      | RegExStar(x) ->
        let x = clean_regex x in
        begin match x with
          | RegExEmpty -> RegExBase ""
          | _ -> RegExStar x
        end
      | _ -> r
    end
  in

  let merge_concated_bases : regex -> regex =
    let rec retrieve_rightmost_concated_base
        (r:regex)
      : (regex option * string option) =
      begin match r with
        | RegExConcat (r1,r2) ->
          begin match retrieve_rightmost_concated_base r2 with
            | (None, so) -> (Some r1, so)
            | (Some r2, so) -> (Some (RegExConcat (r1,r2)),so)
          end
        | RegExBase s -> (None, Some s)
        | _ -> (Some r, None)
      end
    in
    let rec try_insert_into_leftmost_concated_base
        (r:regex)
        (s1:string)
      : regex option =
      begin match r with
        | RegExConcat (r1,r2) ->
          Option.map
            ~f:(fun r1 -> RegExConcat (r1,r2))
            (try_insert_into_leftmost_concated_base r1 s1)
        | RegExBase s2 ->
          Some (RegExBase (s1^s2))
        | _ -> None
      end
    in
    let merge_concated_bases_current_level
        (r:regex)
      : regex =
      begin match r with
        | RegExConcat (r1,r2) ->
          begin match retrieve_rightmost_concated_base r1 with
            | (r1o,Some s1) ->
              begin match try_insert_into_leftmost_concated_base r2 s1 with
                | None -> r
                | Some r2 ->
                  begin match r1o with
                    | None -> r2
                    | Some r1 -> RegExConcat (r1,r2)
                  end
              end
            | (_, None) -> r
          end
        | _ -> r
      end
    in

    apply_at_every_level_regex merge_concated_bases_current_level
  in

  fold_until_fixpoint
    (Fn.compose
       merge_concated_bases
       (Fn.compose
          clean_regex
          maximally_factor_regex))


