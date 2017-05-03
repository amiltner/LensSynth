open Core.Std
open Util
open Printf
open String_utilities
open Permutation




(**** General {{{ *****)

exception Internal_error of string
let internal_error f s = raise @@ Internal_error (sprintf "(%s) %s" f s)

type id = string

(***** }}} *****)


(**** Regex {{{ *****)

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

let rec regex_to_string (r:regex) : string =
  begin match r with
  | RegExEmpty -> "{}"
  | RegExBase s -> "\"" ^ s ^ "\""
  | RegExConcat (r1,r2) -> paren ((regex_to_string r1) ^ "" ^ (regex_to_string r2))
  | RegExOr (r1,r2) -> paren ((regex_to_string r1) ^ "|" ^ (regex_to_string r2))
  | RegExStar (r') -> paren (regex_to_string r') ^ "*"
  | RegExVariable s -> s
  end

let rec regex_compare (r1:regex) (r2:regex) : comparison =
  begin match (r1,r2) with
    | (RegExEmpty,RegExEmpty) -> EQ
    | (RegExEmpty,_) -> LT
    | (_,RegExEmpty) -> GT

    | (RegExBase s1, RegExBase s2) ->
      string_compare s1 s2
    | (RegExBase _, _) -> LT
    | (_, RegExBase _) -> GT

    | (RegExConcat(r11,r12),RegExConcat(r21,r22)) ->
      pair_compare
        regex_compare
        regex_compare
        (r11,r12)
        (r21,r22)
    | (RegExConcat _, _) -> LT
    | (_, RegExConcat _) -> GT

    | (RegExOr(r11,r12),RegExOr(r21,r22)) ->
      pair_compare
        regex_compare
        regex_compare
        (r11,r12)
        (r21,r22)
    | (RegExOr _, _) -> LT
    | (_, RegExOr _) -> GT

    | (RegExStar r1', RegExStar r2') ->
      regex_compare
        r1'
        r2'
    | (RegExStar _, _) -> LT
    | (_, RegExStar _) -> GT

    | (RegExVariable v1, RegExVariable v2) ->
      string_compare
        v1
        v2
  end

let rec regex_hash (r:regex) : int =
  begin match r with
    | RegExEmpty -> 234789
    | RegExBase s -> 2390384 lxor (String.hash s)
    | RegExConcat (r1,r2) -> 345890 lxor (regex_hash r1) lxor (regex_hash r2)
    | RegExOr (r1,r2) -> 527039 lxor (regex_hash r1) lxor (regex_hash r2)
    | RegExStar r' -> -128947 lxor (regex_hash r')
    | RegExVariable s -> 14967827 lxor (String.hash s)
  end
(***** }}} *****)




(**** Lens {{{ *****)

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
  | LensPermute of Permutation.t * (lens list)


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

let rec lens_to_string (l:lens) : string =
  begin match l with
  | LensConst (s1,s2) -> "const('" ^ s1 ^ "','" ^ s2 ^ "')"
  | LensConcat (l1,l2) -> paren ((lens_to_string l1) ^ "." ^ (lens_to_string l2))
  | LensCompose (l1,l2) -> paren ((lens_to_string l1) ^ ";" ^ (lens_to_string l2))
  | LensSwap (l1,l2) -> "swap(" ^ (lens_to_string l1) ^ "," ^ (lens_to_string l2) ^ ")"
  | LensUnion (l1,l2) -> paren ((lens_to_string l1) ^ "|" ^ (lens_to_string l2))
  | LensIterate (l') -> paren (lens_to_string l') ^ "*"
  | LensIdentity r -> "id(" ^ (regex_to_string r) ^")"
  | LensInverse l' -> "inverse(" ^ (lens_to_string l') ^ ")"
  | LensVariable n -> n
  | LensPermute (p,ls) -> "permute" ^
                          ((String_utilities.string_of_pair
                              Permutation.pp
                              (String_utilities.string_of_list lens_to_string))
                             (p,ls))
  end

let rec lens_size (l:lens) : int =
  begin match l with
    | LensConst _ -> 1
    | LensConcat (l1,l2) ->
      1 + (lens_size l1) + (lens_size l2)
    | LensCompose (l1,l2) ->
      1 + (lens_size l1) + (lens_size l2)
    | LensSwap (l1,l2) ->
      1 + (lens_size l1) + (lens_size l2)
    | LensUnion (l1,l2) ->
      1 + (lens_size l1) + (lens_size l2)
    | LensIterate (l') ->
      1 + (lens_size l')
    | LensIdentity _ -> 1
    | LensInverse l' ->
      1 + (lens_size l')
    | LensVariable _ -> 1
    | LensPermute (_,ls) ->
      1 + (List.fold_left
             ~f:(fun acc l' -> acc + (lens_size l'))
             ~init:0
             ls)
  end


let rec lens_compare (l1:lens) (l2:lens) : comparison =
  begin match (l1,l2) with
    | (LensConst (s11,s12),LensConst (s21,s22)) ->
      pair_compare
        string_compare
        string_compare
        (s11,s12)
        (s21,s22)
    | (LensConst _, _) -> LT
    | (_, LensConst _) -> GT

    | (LensConcat (l11,l12),LensConcat (l21,l22)) ->
      pair_compare
        lens_compare
        lens_compare
        (l11,l12)
        (l21,l22)
    | (LensConcat _, _) -> LT
    | (_, LensConcat _) -> GT

    | (LensSwap (l11,l12),LensSwap (l21,l22)) ->
      pair_compare
        lens_compare
        lens_compare
        (l11,l12)
        (l21,l22)
    | (LensSwap _, _) -> LT
    | (_, LensSwap _) -> GT

    | (LensUnion (l11,l12),LensUnion (l21,l22)) ->
      pair_compare
        lens_compare
        lens_compare
        (l11,l12)
        (l21,l22)
    | (LensUnion _, _) -> LT
    | (_, LensUnion _) -> GT

    | (LensCompose (l11,l12),LensCompose (l21,l22)) ->
      pair_compare
        lens_compare
        lens_compare
        (l11,l12)
        (l21,l22)
    | (LensCompose _, _) -> LT
    | (_, LensCompose _) -> GT

    | (LensIterate l1',LensIterate l2') ->
      lens_compare
        l1'
        l2'
    | (LensIterate _, _) -> LT
    | (_, LensIterate _) -> GT

    | (LensIdentity r1,LensIdentity r2) ->
      regex_compare
        r1
        r2
    | (LensIdentity _, _) -> LT
    | (_, LensIdentity _) -> GT

    | (LensInverse l1',LensInverse l2') ->
      lens_compare
        l1'
        l2'
    | (LensInverse _, _) -> LT
    | (_, LensInverse _) -> GT

    | (LensVariable v1,LensVariable v2) ->
      string_compare
        v1
        v2
    | (LensVariable _, _) -> LT
    | (_, LensVariable _) -> GT

    | (LensPermute (p1,ls1),LensPermute (p2,ls2)) ->
      pair_compare
        Permutation.compare
        (compare_list ~cmp:lens_compare)
        (p1,ls1)
        (p2,ls2)
  end

let rec lens_hash (l:lens) : int =
  begin match l with
    | LensConst (s1,s2) -> 58129320 lxor (String.hash s1) lxor (String.hash s2)
    | LensConcat (l1,l2) -> 912812382 lxor (lens_hash l1) lxor (lens_hash l2)
    | LensSwap (l1,l2) -> -12899379 lxor (lens_hash l1) lxor (lens_hash l2)
    | LensUnion (l1,l2) -> 18912899 lxor (lens_hash l1) lxor (lens_hash l2)
    | LensCompose (l1,l2) -> -019092 lxor (lens_hash l1) lxor (lens_hash l2)
    | LensIterate l' -> 212893489 lxor (lens_hash l')
    | LensIdentity r -> 3828910 lxor (regex_hash r)
    | LensInverse l -> 20920910 lxor (lens_hash l)
    | LensVariable s -> 28945929 lxor (String.hash s)
    | LensPermute (p,ls) ->
      1390903
      lxor (Permutation.hash p)
      lxor (List.foldi
              ~f:(fun i acc l ->
                  (lens_hash l)
                  lxor (Int.hash i)
                  lxor acc)
              ~init:1237662)
        ls
  end

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

(***** }}} *****)



(**** Language {{{ *****)

type examples = (string * string) list

type specification = (string * regex * regex * (string * string) list)

type declaration =
  | DeclRegexCreation of (id * regex * bool)
  | DeclTestString of (regex * string)
  | DeclSynthesizeLens of specification
  | DeclLensCreation of id * regex * regex * lens
  | DeclTestLens of id * examples

type program = declaration list

type synth_problems = (string * regex * bool) list * (specification list) 

(***** }}} *****)


