open Core.Std
open OUnit
open Eval
open Lens
open Lang
open Pp
open Gen
open Permutation

(* Lang tests *)
let test_normalizization (expected:normalized_regex) (actual:normalized_regex) =
  assert_equal
    ~printer:pp_normalized_regex
    expected
    actual

let test_to_normalized_exp_base _ =
  test_normalizization
    (to_normalized_exp (RegExBase "x"))
    [[NRXBase "x"]]

let test_to_normalized_exp_concat_easy _ =
  test_normalizization
    [[NRXBase "a"; NRXBase "b"]]
    (to_normalized_exp
      (RegExConcat
        (RegExBase "a",
        RegExBase "b")))
    
let test_to_normalized_exp_concat_tree _ =
  test_normalizization
    [[NRXBase "a"; NRXBase "b"; NRXBase "c"; NRXBase "d"; NRXBase "e"]]
    (to_normalized_exp
      (RegExConcat
        (RegExConcat
          (RegExBase "a",
          RegExBase "b"),
        RegExConcat
          (RegExBase "c",
          RegExConcat
            (RegExBase "d",
            RegExBase "e")))))

let test_to_normalized_exp_union_tree _ =
  test_normalizization
    [[NRXBase "a"]; [NRXBase "b"]; [NRXBase "c"]; [NRXBase "d"]; [NRXBase "e"]]
    (to_normalized_exp
      (RegExOr
        (RegExOr
          (RegExBase "a",
          RegExBase "b"),
        RegExOr
          (RegExBase "c",
          RegExOr
            (RegExBase "d",
            RegExBase "e")))))

let test_to_normalized_exp_append_distributeconcat _ =
  test_normalizization
    (to_normalized_exp
      (RegExConcat
        (RegExConcat
          (RegExBase "a",
          RegExOr
            (RegExBase "b",
            RegExBase "c")),
        RegExBase "d")))
    [[NRXBase "a"; NRXBase "b"; NRXBase "d"];
     [NRXBase "a"; NRXBase "c"; NRXBase "d"]]

let test_to_normalized_exp_complicated _ =
  test_normalizization
    (to_normalized_exp
    (RegExConcat
      (RegExConcat
        (RegExBase "a",
          RegExOr
            (RegExBase "b",
            RegExStar
              (RegExConcat
                (RegExConcat
                  (RegExBase "c",
                  RegExOr
                    (RegExBase "d",
                    RegExBase "e")),
                RegExBase "f")))),
         RegExBase "g")))
    [[NRXBase "a"; NRXBase "b"; NRXBase "g"];
     [NRXBase "a";
      NRXStar [
        [NRXBase "c"; NRXBase "d"; NRXBase "f"];
        [NRXBase "c"; NRXBase "e"; NRXBase "f"]
      ];
      NRXBase "g"]]


let to_normalized_exp_suite = "to_normalized_exp Unit Tests" >:::
  ["test_to_normalized_exp_base" >:: test_to_normalized_exp_base;
   "test_to_normalized_exp_concat_easy" >:: test_to_normalized_exp_concat_easy;
   "test_to_normalized_exp_concat_tree" >:: test_to_normalized_exp_concat_tree;
   "test_to_normalized_exp_union_tree" >:: test_to_normalized_exp_union_tree;
   "test_to_normalized_exp_append_distributeconcat" >:: test_to_normalized_exp_append_distributeconcat;
   "test_to_normalized_exp_complicated" >:: test_to_normalized_exp_complicated]

let _ = run_test_tt_main to_normalized_exp_suite


(* Eval tests *)
let test_bools (expected:bool) (actual:bool) =
  assert_equal
    ~printer:string_of_bool
    expected
    actual

let test_eval_regex_base_positive _ =
  test_bools
    true
    (eval_regex [] (RegExBase "x") "x")

let test_eval_regex_base_negative1 _ =
  test_bools
    false
    (eval_regex [] (RegExBase "x") "y")

let test_eval_regex_base_negative2 _ =
  test_bools
    false
    (eval_regex [] (RegExBase "x") "xx")

let test_eval_regex_concat_positive1 _ =
  test_bools
    true
    (eval_regex []
      (RegExConcat
        (RegExBase "x",
        RegExConcat
          (RegExBase "y",
          RegExBase "z"))) "xyz")

let test_eval_regex_concat_positive2 _ =
  test_bools
    true
    (eval_regex []
      (RegExConcat
        (RegExConcat
          (RegExBase "x",
          RegExBase "y"),
        RegExBase "z")) "xyz")

let test_eval_regex_concat_negative1 _ =
  test_bools
    false
    (eval_regex []
      (RegExConcat
        (RegExBase "x",
        RegExBase "y")) "x")

let test_eval_regex_concat_negative2 _ =
  test_bools
    false
    (eval_regex []
      (RegExConcat
        (RegExBase "x",
        RegExBase "y")) "xz")

let test_eval_regex_concat_negative3 _ =
  test_bools
    false
    (eval_regex []
      (RegExConcat
        (RegExBase "x",
        RegExBase "y")) "yx")

let test_eval_regex_concat_negative4 _ =
  test_bools
    false
    (eval_regex []
      (RegExConcat
        (RegExBase "x",
        RegExBase "y")) "xyz")

let test_eval_regex_or_positive _ =
  test_bools
    true
    (eval_regex []
      (RegExOr
        (RegExOr
          (RegExBase "a",
          RegExBase "b"),
        (RegExOr
          (RegExBase "c",
          RegExBase "d")))) "c")

let test_eval_regex_or_negative _ =
  test_bools
    false
    (eval_regex []
      (RegExOr
        (RegExOr
          (RegExBase "a",
          RegExBase "b"),
        (RegExOr
          (RegExBase "c",
          RegExBase "d")))) "x")

let test_eval_regex_star_empty _ =
  test_bools
    true
    (eval_regex []
      (RegExStar
        (RegExBase "a")) "")

let test_eval_regex_star_one _ =
  test_bools
    true
    (eval_regex []
      (RegExStar
        (RegExBase "a")) "a")

let test_eval_regex_star_two _ =
  test_bools
    true
    (eval_regex []
      (RegExStar
        (RegExBase "a")) "aa")

let test_eval_regex_star_negative1 _ =
  test_bools
    false
    (eval_regex []
      (RegExStar
        (RegExBase "a")) "b")

let test_eval_regex_star_negative2 _ =
  test_bools
    false
    (eval_regex []
      (RegExStar
        (RegExBase "a")) "ab")

let test_eval_regex_star_negative3 _ =
  test_bools
    false
    (eval_regex []
      (RegExStar
        (RegExBase "a")) "ba")


let eval_regex_suite = "eval_regex Unit Tests" >:::
  ["test_eval_regex_base_positive" >:: test_eval_regex_base_positive;
   "test_eval_regex_base_negative1" >:: test_eval_regex_base_negative1;
   "test_eval_regex_base_negative2" >:: test_eval_regex_base_negative2;
   "test_eval_regex_concat_positive1" >:: test_eval_regex_concat_negative4;
   "test_eval_regex_concat_positive2" >:: test_eval_regex_concat_negative4;
   "test_eval_regex_concat_negative1" >:: test_eval_regex_concat_negative4;
   "test_eval_regex_concat_negative2" >:: test_eval_regex_concat_negative4;
   "test_eval_regex_concat_negative3" >:: test_eval_regex_concat_negative4;
   "test_eval_regex_concat_negative4" >:: test_eval_regex_concat_negative4;
   "test_eval_regex_or_negative" >:: test_eval_regex_or_negative;
   "test_eval_regex_or_positive" >:: test_eval_regex_or_positive;
   "test_eval_regex_star_empty" >:: test_eval_regex_star_empty;
   "test_eval_regex_star_one" >:: test_eval_regex_star_one;
   "test_eval_regex_star_two" >:: test_eval_regex_star_two;
   "test_eval_regex_star_negative1" >:: test_eval_regex_star_negative1;
   "test_eval_regex_star_negative2" >:: test_eval_regex_star_negative2;
   "test_eval_regex_star_negative3" >:: test_eval_regex_star_negative3;
  ]

let _ = run_test_tt_main eval_regex_suite

let test_string_options (expected:string option) (actual:string option) =
  assert_equal
    ~printer:(fun x -> begin match x with | None -> "None" | Some s -> s end)
    expected
    actual

let test_eval_lens_const_basic_positive _ =
  test_string_options
    (Some "b")
    (eval_lens (ConstLens ("a","b")) "a")

let test_eval_lens_const_basic_negative _ =
  test_string_options
    (None)
    (eval_lens (ConstLens ("a","b")) "b")

let eval_lens_suite = "eval_lens Unit Tests" >:::
  ["test_eval_lens_const_basic_positive" >:: test_eval_lens_const_basic_positive;
   "test_eval_lens_const_basic_negative" >:: test_eval_lens_const_basic_negative;
  ]

let _ = run_test_tt_main eval_lens_suite

let test_string_double_option
  (expected:(string*string) option) (actual:(string*string) option) =
  assert_equal
    ~printer:(fun x -> begin match x with
              | None -> "None"
              | Some (x1,x2) -> "(" ^ x1 ^ "," ^ x2 ^ ")" end)
    expected
    actual

let test_retrieve_regex_concat_split _ =
  test_string_double_option
    (Some ("a","b"))
    (retrieve_regex_concat_split [] (RegExBase "a") (RegExBase "b") "ab")

let retrieve_regex_concat_split_suite = "retrieve_regex_concat_split Unit Tests" >:::
  ["test_retrieve_regex_concat_split" >:: test_retrieve_regex_concat_split;
  ]

let _ = run_test_tt_main retrieve_regex_concat_split_suite

let test_string_list_option
  (expected:(string list) option) (actual:(string list) option) =
  assert_equal
    ~printer:(fun x -> begin match x with
              | None -> "None"
              | Some xs -> "[" ^ (String.concat xs ~sep:";") ^ "]" end)
    expected
    actual

let test_retrieve_regex_star_splits _ =
  test_string_list_option
    (Some ["a";"a"])
    (retrieve_regex_star_splits [] (RegExBase "a") "aa")

let retrieve_regex_star_splits_suite = "retrieve_regex_star_splits Unit Tests" >:::
  ["test_retrieve_regex_star_splits" >:: test_retrieve_regex_star_splits;
  ]

let _ = run_test_tt_main retrieve_regex_star_splits_suite

let test_lens_list (expected:lens list) (actual:lens list) =
  assert_equal
    ~printer:(fun ls -> String.concat (List.map ~f:pp_lens ls) ~sep:"\n")
    expected
    actual

let test_gen_lenses_const_nosoln _ =
  test_lens_list
    []
    (gen_lenses [] (RegExBase "x") (RegExBase "y") [("a","b")])

let test_gen_lenses_const_soln _ =
  test_lens_list
    [ConstLens ("x","y")]
    (gen_lenses [] (RegExBase "x") (RegExBase "y") [("x","y")])

let test_gen_lenses_const_identity _ =
  test_lens_list
    [IdentityLens; ConstLens ("x","x")]
    (gen_lenses [] (RegExBase "x") (RegExBase "x") [("x","x")])

let test_gen_lenses_userdef_ident _ =
  test_lens_list
    [IdentityLens]
    (gen_lenses
      [("A",RegExBase "a")]
      (RegExUserDefined "A")
      (RegExUserDefined "A")
      [("a","a")])

let test_gen_lenses_userdef_mistype _ =
  test_lens_list
    []
    (gen_lenses
      [("A",RegExBase "a"); ("B",RegExBase "a")]
      (RegExUserDefined "A")
      (RegExUserDefined "B")
      [("a","a")])

let test_gen_lenses_concat _ =
  test_lens_list
    [ConcatLens (ConstLens ("a","x"), ConstLens ("b","y"));
      SwapLens (ConstLens ("a","y"), ConstLens ("b","x"))]
    (gen_lenses
      []
      (RegExConcat (RegExBase "a", RegExBase "b"))
      (RegExConcat (RegExBase "x", RegExBase "y"))
      [("ab","xy")])

let test_gen_lenses_swap _ =
  test_lens_list
    [SwapLens (IdentityLens, IdentityLens)]
    (gen_lenses
      [("A",RegExBase "a");("B",RegExBase "b")]
      (RegExConcat (RegExUserDefined "A", RegExUserDefined "B"))
      (RegExConcat (RegExUserDefined "B", RegExUserDefined "A"))
      [("ab","ba")])

let test_gen_lenses_union _ =
  test_lens_list
    [UnionLens (IdentityLens, ConstLens("b","c"))]
    (gen_lenses
      [("A",RegExBase "a")]
      (RegExOr (RegExUserDefined "A", RegExBase "b"))
      (RegExOr (RegExUserDefined "A", RegExBase "c"))
      [])

let test_gen_lenses_star _ =
  test_lens_list
    [IterateLens (ConstLens("a","b"))]
    (gen_lenses
      []
      (RegExStar (RegExBase "a"))
      (RegExStar (RegExBase "b"))
      [("aa","bb")])

let gen_lens_suite = "gen_lens Unit Tests" >:::
  ["test_gen_lenses_const_nosoln" >:: test_gen_lenses_const_nosoln;
   "test_gen_lenses_soln" >:: test_gen_lenses_const_soln;
   "test_gen_lenses_const_identity" >:: test_gen_lenses_const_identity;
   "test_gen_lenses_userdef_ident" >:: test_gen_lenses_userdef_ident;
   "test_gen_lenses_userdef_mistype" >:: test_gen_lenses_userdef_mistype;
   "test_gen_lenses_concat" >:: test_gen_lenses_concat;
   "test_gen_lenses_swap" >:: test_gen_lenses_swap;
   "test_gen_lenses_union" >:: test_gen_lenses_union;
   "test_gen_lenses_star" >:: test_gen_lenses_star;
  ]

let _ = run_test_tt_main gen_lens_suite

let test_int = assert_equal ~printer:string_of_int

let test_permutation_create_invalid_0 _ =
  assert_raises
    (Failure "Not Bijection")
    (fun _ -> Permutation.create [0;2])

let test_permutation_create_invalid_1 _ =
  assert_raises
    (Failure "Not Bijection")
    (fun _ -> Permutation.create [0;0])

let test_permutation_create_invalid_2 _ =
  assert_raises
    (Failure "Not Bijection")
    (fun _ -> Permutation.create [-1])

let test_permutation_apply_identity _ =
  test_int
    2
    (Permutation.apply (Permutation.create [0;1;2]) 2)

let test_permutation_apply_nonidentity _ =
  test_int
    0
    (Permutation.apply (Permutation.create [1;2;0]) 2)
    
let test_permutation_apply_invalid _ =
  assert_raises
    (Failure "out of range")
    (fun _ -> Permutation.apply (Permutation.create []) 0)

let test_permutation_apply_inverse_identity _ =
  test_int
    2
    (Permutation.apply_inverse (Permutation.create [0;1;2]) 2)

let test_permutation_apply_inverse_nonidentity _ =
  test_int
    1
    (Permutation.apply_inverse (Permutation.create [1;2;0]) 2)
    
let test_permutation_apply_inverse_invalid _ =
  assert_raises
    (Failure "out of range")
    (fun _ -> Permutation.apply_inverse (Permutation.create []) 0)

let test_permutation_create_all _ =
  test_int
    2
    (List.length (Permutation.create_all 2))

let permutation_suite = "permutation Unit Tests" >:::
  ["test_permutation_create_invalid_0" >:: test_permutation_create_invalid_0;
   "test_permutation_create_invalid_1" >:: test_permutation_create_invalid_1;
   "test_permutation_create_invalid_2" >:: test_permutation_create_invalid_2;
   "test_permutation_apply_identity" >:: test_permutation_apply_identity;
   "test_permutation_apply_nonidentity" >:: test_permutation_apply_nonidentity;
   "test_permutation_apply_invalid" >:: test_permutation_apply_invalid;
   "test_permutation_apply_inverse_identity" >:: test_permutation_apply_inverse_identity;
   "test_permutation_apply_inverse_nonidentity" >:: test_permutation_apply_inverse_nonidentity;
   "test_permutation_apply_inverse_invalid" >:: test_permutation_apply_inverse_invalid;
   "test_permutation_create_all" >:: test_permutation_create_all;
  ]

let _ = run_test_tt_main permutation_suite
