open String_utilities

type regex =
  | RegExEmpty
  | RegExBase of string
  | RegExConcat of regex * regex
  | RegExOr of regex * regex
  | RegExStar of regex
  | RegExVariable of string

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

let rec string_of_regex (r:regex) : string =
  begin match r with
  | RegExEmpty -> "{}"
  | RegExBase s -> "\"" ^ s ^ "\""
  | RegExConcat (r1,r2) -> paren ((string_of_regex r1) ^ "" ^ (string_of_regex r2))
  | RegExOr (r1,r2) -> paren ((string_of_regex r1) ^ "|" ^ (string_of_regex r2))
  | RegExStar (r') -> paren (string_of_regex r') ^ "*"
  | RegExVariable s -> s
  end
