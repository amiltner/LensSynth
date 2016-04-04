open Core.Std
open Permutation
open Util
open OUnit
open Eval
open Lens
open Lang
open Pp
open Gen

(* Lang tests *)
let test_dnf (expected:dnf_regex) (actual:dnf_regex) =
  assert_equal
    ~printer:pp_dnf_regex_as_regex
    expected
    actual

let test_int = assert_equal ~printer:string_of_int

let test_int_option =
  assert_equal
    ~printer:(fun int_option -> begin match int_option with
    | None -> "None"
    | Some x -> string_of_int x
    end)

let test_string_double_option
  (expected:(string*string) option) (actual:(string*string) option) =
  assert_equal
    ~printer:(fun x -> begin match x with
              | None -> "None"
              | Some (x1,x2) -> "(" ^ x1 ^ "," ^ x2 ^ ")" end)
    expected
    actual

let test_string_list_option
  (expected:(string list) option) (actual:(string list) option) =
  assert_equal
    ~printer:(fun x -> begin match x with
              | None -> "None"
              | Some xs -> "[" ^ (String.concat xs ~sep:";") ^ "]" end)
    expected
    actual

let test_comparison =
  assert_equal
    ~printer:Pp.pp_comparison

let test_permutation_option =
  assert_equal
    ~printer:(fun x -> begin match x with
                       | None -> "None"
                       | Some p -> "Some " ^ (Permutation.pp p)
                       end)

let test_permutation_guesses_option =
  assert_equal
    ~printer:(fun x -> begin match x with
              | None -> "None"
              | Some (p,g) ->
                  "Some (" ^
                  (Permutation.pp p) ^ "   ,   [" ^
                  String.concat
                    ~sep:" ; "
                    (List.map
                      ~f:(fun (i,j) -> (string_of_int i) ^ "->" ^ (string_of_int
                      j))
                      g)
                    ^ "])"
    end)

let test_exampled_dnf_option =
  assert_equal
    ~printer:(fun ro ->
      begin match ro with
      | None -> "None"
      | Some r -> Pp.pp_exampled_dnf_regex r
      end)

let test_to_normalized_exp_base _ =
  test_dnf
    (to_dnf_regex (RegExBase "x"))
    [([],["x"])]

let test_to_normalized_exp_concat_easy _ =
  test_dnf
    [([],["ab"])]
    (to_dnf_regex
      (RegExConcat
        (RegExBase "a",
        RegExBase "b")))
    
let test_to_normalized_exp_concat_tree _ =
  test_dnf
    [([],["abcde"])]
    (to_dnf_regex
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
  test_dnf
    [([],["a"]);([],["b"]);([],["c"]);([],["d"]);([],["e"])]
    (to_dnf_regex
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
  test_dnf
    (to_dnf_regex
      (RegExConcat
        (RegExConcat
          (RegExBase "a",
          RegExOr
            (RegExBase "b",
            RegExBase "c")),
        RegExBase "d")))
    [([],["abd"]);([],["acd"])]

let test_to_normalized_exp_complicated _ =
  test_dnf
  ([([],["abg"]);([AStar ([([],["cdf"]);([],["cef"])])],["a";"g"])])
    (to_dnf_regex
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


let to_normalized_exp_suite = "to_normalized_exp Unit Tests" >:::
  ["test_to_normalized_exp_base" >:: test_to_normalized_exp_base;
   "test_to_normalized_exp_concat_easy" >:: test_to_normalized_exp_concat_easy;
   "test_to_normalized_exp_concat_tree" >:: test_to_normalized_exp_concat_tree;
   "test_to_normalized_exp_union_tree" >:: test_to_normalized_exp_union_tree;
   "test_to_normalized_exp_append_distributeconcat" >:: test_to_normalized_exp_append_distributeconcat;
   "test_to_normalized_exp_complicated" >:: test_to_normalized_exp_complicated]

let _ = run_test_tt_main to_normalized_exp_suite

let test_compare_dnf_regexs_userdefineds_eq _ =
  test_comparison
    EQ
    (compare_dnf_regexs [[AUserDefined "a"],["";"1qaz"]]
    [[AUserDefined "a"],["";"2wsx"]])

let test_compare_dnf_regexs_userdefineds_lt _ =
  test_comparison
    LT
    (compare_dnf_regexs [[AUserDefined "a"],["";"1qaz"]]
    [[AUserDefined "b"],["";"2wsx"]])

let compare_dnf_regexs_suite = "compare_dnf_regexs Unit Tests" >:::
  ["test_compare_dnf_regexs_userdefineds_eq" >:: test_compare_dnf_regexs_userdefineds_eq;
   "test_compare_dnf_regexs_userdefineds_lt" >:: test_compare_dnf_regexs_userdefineds_lt;
  ]

let _ = run_test_tt_main compare_dnf_regexs_suite


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

let test_retrieve_regex_concat_split _ =
  test_string_double_option
    (Some ("a","b"))
    (retrieve_regex_concat_split [] (RegExBase "a") (RegExBase "b") "ab")

let retrieve_regex_concat_split_suite = "retrieve_regex_concat_split Unit Tests" >:::
  ["test_retrieve_regex_concat_split" >:: test_retrieve_regex_concat_split;
  ]

let _ = run_test_tt_main retrieve_regex_concat_split_suite

let test_retrieve_regex_star_splits _ =
  test_string_list_option
    (Some ["a";"a"])
    (retrieve_regex_star_splits [] (RegExBase "a") "aa")

let retrieve_regex_star_splits_suite = "retrieve_regex_star_splits Unit Tests" >:::
  ["test_retrieve_regex_star_splits" >:: test_retrieve_regex_star_splits;
  ]

let _ = run_test_tt_main retrieve_regex_star_splits_suite

let test_retrieve_dnf_clause_choices _ =
  test_int_option
    (Some 1)
    (retrieve_dnf_clause_choices [] [([],["a"]);([],["b"])] "b")

let retrieve_dnf_clause_choices_suite = "retrieve_dnf_clause_choices Unit Tests" >:::
  ["test_retrieve_dnf_clause_choices" >:: test_retrieve_dnf_clause_choices;
  ]

let _ = run_test_tt_main retrieve_dnf_clause_choices_suite

let test_retrieve_atom_splits _ =
  test_string_list_option
    (Some ["a";"b"])
    (retrieve_atom_splits [("A",RegExBase "a");("B",RegExBase "b")]
    ([AUserDefined "A"; AUserDefined "B"],["qwer";"asdf";"zxcv"])
    "qweraasdfbzxcv")

let test_retrieve_atom_splits_star _ =
  test_string_list_option
    (Some [""])
    (retrieve_atom_splits [] ([AStar [([],["a"])]],["a";""]) "a")

let retrieve_atom_splits_suite = "retrieve_atom_splits Unit Tests" >:::
  ["test_retrieve_atom_splits" >:: test_retrieve_atom_splits;
   "test_retrieve_atom_splits_star" >:: test_retrieve_atom_splits_star;
  ]

let _ = run_test_tt_main retrieve_atom_splits_suite

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


let test_int_list = assert_equal
  ~printer:(fun is -> String.concat (List.map ~f:string_of_int is) ~sep:",")

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

let test_permutation_create_from_doubles_invalid_0 _ =
  assert_raises
    (Failure "Not Bijection")
    (fun _ -> Permutation.create_from_doubles [(0,2);(1,0)])

let test_permutation_create_from_doubles_invalid_1 _ =
  assert_raises
    (Failure "Not Bijection")
    (fun _ -> Permutation.create_from_doubles [(0,1);(0,0)])

let test_permutation_create_from_doubles_invalid_2 _ =
  assert_raises
    (Failure "Not Bijection")
    (fun _ -> Permutation.create_from_doubles [(0,1);(1,1)])

let test_permutation_create_from_constraints_none_0 _ =
  test_permutation_guesses_option
    None
    (Permutation.create_from_constraints 3 [(0,2);(0,1);(0,0)] [])

let test_permutation_create_from_constraints_none_1 _ =
  test_permutation_guesses_option
    None
    (Permutation.create_from_constraints 3 [(0,0);(1,0);(2,0)] [])

let test_permutation_create_from_constraints_identity _ =
  test_permutation_guesses_option
    (Some ((Permutation.create [0;1;2]), [(0,0);(1,1);(2,2)]))
    (Permutation.create_from_constraints 3 [] [])

let test_permutation_create_from_constraints_withvalid _ =
  test_permutation_guesses_option
    (Some ((Permutation.create [1;0;2]), [(1,0);(2,2)]))
    (Permutation.create_from_constraints 3 [] [(0,1)])

let test_permutation_create_from_constraints_withinvalid _ =
  test_permutation_guesses_option
    (Some ((Permutation.create [1;0;2]), [(0,1);(1,0);(2,2)]))
    (Permutation.create_from_constraints 3 [(0,0)] [])

let test_permutation_apply_identity _ =
  test_int
     2
     (Permutation.apply (Permutation.create [0;1;2]) 2)

let test_permutation_apply_nonidentity _ =
  test_int
    1
    (Permutation.apply (Permutation.create [1;2;0]) 2)

let test_permutation_apply_from_doubles _ =
  test_int
    0
    (Permutation.apply
      (Permutation.create_from_doubles [(1,2);(2,0);(0,1)])
      2)
                
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
    0
    (Permutation.apply_inverse (Permutation.create [1;2;0]) 2)
                
let test_permutation_apply_inverse_invalid _ =
  assert_raises
    (Failure "out of range")
    (fun _ -> Permutation.apply_inverse (Permutation.create []) 0)

let test_permutation_create_all _ =
  test_int
    2
    (List.length (Permutation.create_all 2))

let test_permutation_apply_to_list_identity _ =
  test_int_list
    [2;4;8]
    (Permutation.apply_to_list_exn (Permutation.create [0;1;2]) [2;4;8])

let test_permutation_apply_to_list_c3 _ =
  test_int_list
    [8;2;4]
    (Permutation.apply_to_list_exn (Permutation.create [2;0;1]) [2;4;8])

let test_permutation_apply_inverse_to_list_identity _ =
  test_int_list
    [2;4;8]
    (Permutation.apply_inverse_to_list_exn (Permutation.create [0;1;2]) [2;4;8])

let test_permutation_apply_inverse_to_list_c3 _ =
  test_int_list
    [4;8;2]
    (Permutation.apply_inverse_to_list_exn (Permutation.create [2;0;1]) [2;4;8])

let permutation_suite = "permutation Unit Tests" >:::
  ["test_permutation_create_invalid_0" >:: test_permutation_create_invalid_0;
   "test_permutation_create_invalid_1" >:: test_permutation_create_invalid_1;
   "test_permutation_create_invalid_2" >:: test_permutation_create_invalid_2;
   "test_permutation_create_from_doubles_invalid_0" >:: test_permutation_create_from_doubles_invalid_0;
   "test_permutation_create_from_doubles_invalid_1" >:: test_permutation_create_from_doubles_invalid_1;
   "test_permutation_create_from_doubles_invalid_2" >:: test_permutation_create_from_doubles_invalid_2;
   "test_permutation_create_from_constraints_none_0" >:: test_permutation_create_from_constraints_none_0;
   "test_permutation_create_from_constraints_none_1" >:: test_permutation_create_from_constraints_none_1;
   "test_permutation_create_from_constraints_identity" >:: test_permutation_create_from_constraints_identity;
   "test_permutation_create_from_constraints_withvalid" >:: test_permutation_create_from_constraints_withvalid;
   "test_permutation_create_from_constraints_withinvalid" >:: test_permutation_create_from_constraints_withinvalid;
   "test_permutation_apply_identity" >:: test_permutation_apply_identity;
   "test_permutation_apply_nonidentity" >:: test_permutation_apply_nonidentity;
   "test_permutation_apply_from_doubles" >:: test_permutation_apply_from_doubles;
   "test_permutation_apply_invalid" >:: test_permutation_apply_invalid;
   "test_permutation_apply_inverse_identity" >:: test_permutation_apply_inverse_identity;
   "test_permutation_apply_inverse_nonidentity" >:: test_permutation_apply_inverse_nonidentity;
   "test_permutation_apply_inverse_invalid" >:: test_permutation_apply_inverse_invalid;
   "test_permutation_create_all" >:: test_permutation_create_all;
   "test_permutation_apply_to_list_identity" >:: test_permutation_apply_to_list_identity;
   "test_permutation_apply_to_list_c3" >:: test_permutation_apply_to_list_c3;
   "test_permutation_apply_inverse_to_list_identity" >:: test_permutation_apply_inverse_to_list_identity;
   "test_permutation_apply_inverse_to_list_c3" >:: test_permutation_apply_inverse_to_list_c3;
  ]

let _ = run_test_tt_main permutation_suite

let test_char_list_list = assert_equal
  ~printer:(fun is ->
    (String.concat
    (["[" ^ "]"])
      ~sep:"  ,  ")
  )

let test_char_list_double = assert_equal

let test_bucketize_pairs_symmetric _ =
  test_char_list_list
    [[];['a'];[]]
    (bucketize_pairs 3 [('a',1)])

let test_bucketize_pairs_asymmetric _ =
  test_char_list_list
    [['a'];['b';'c'];[]]
    (bucketize_pairs 3 [('b',1);('a',0);('c',1)])

let test_sub0_error_split_at_index_exn _ =
  assert_raises
    (Failure "invalid index")
    (fun _ -> split_at_index_exn [] (-1))

let test_overlen_error_split_at_index_exn _ =
  assert_raises
    (Failure "invalid index")
    (fun _ -> split_at_index_exn [] (7))

let test_0index_split_at_index_exn _ =
  test_char_list_double
    ([],['a'])
    (split_at_index_exn ['a'] 0)

let test_index_split_at_index_exn _ =
  test_char_list_double
    (['a'],['b'])
    (split_at_index_exn ['a';'b'] 1)

let test_sort_and_partition _ =
  test_char_list_list
    [['a';'a'];['b'];['c';'c';'c'];['z']]
    (sort_and_partition
      (fun x y -> compare_ints (Char.to_int x) (Char.to_int y))
      ['a';'z';'b';'c';'c';'a';'c'])

let util_suite = "Util Unit Tests" >:::
  ["test_bucketize_pairs_symmetric" >:: test_bucketize_pairs_symmetric;
   "test_bucketize_pairs_asmemetric" >:: test_bucketize_pairs_asymmetric;
   "test_sub0_error_split_at_index_exn" >:: test_sub0_error_split_at_index_exn;
   "test_0index_split_at_index_exn" >:: test_0index_split_at_index_exn;
   "test_index_split_at_index_exn" >:: test_index_split_at_index_exn;
   "test_sort_and_partition" >:: test_sort_and_partition;
  ]

let test_sort_and_partition _ =
  test_char_list_list

let _ = run_test_tt_main util_suite

let test_dnf_lens =
  assert_equal
  ~printer:Pp.pp_dnf_regex_as_regex

let test_expand_atom_empty_basic _ =
  test_dnf_lens
    (to_dnf_regex (RegExOr (RegExBase "",RegExConcat (RegExBase "a",RegExStar
    (RegExBase "a")))))
    (expand_atom_rewrite empty_or_not_star_expansion
      (to_dnf_regex (RegExStar (RegExBase "a")))
      0)

let test_expand_atom_empty_concat _ =
  test_dnf_lens
    (to_dnf_regex (RegExOr (RegExBase "x", RegExConcat (RegExBase "xa",
    RegExStar (RegExBase "a")))))
    (expand_atom_rewrite empty_or_not_star_expansion
      (to_dnf_regex (RegExConcat (RegExBase "x", RegExStar (RegExBase "a"))))
      0)

let test_expand_atom_empty_concat_harder _ =
  test_dnf_lens
    (to_dnf_regex
      (RegExOr
        (RegExConcat
          (RegExConcat
            (RegExBase "x", RegExUserDefined "Z")
          ,RegExConcat
            (RegExBase "m", RegExBase "q"))
        ,RegExConcat
          (RegExBase "x",
          RegExConcat (RegExUserDefined "Z",
          RegExConcat (RegExBase "ma",
          RegExConcat (RegExUserDefined "T",
          RegExConcat (RegExBase "p",
          RegExConcat (RegExStar
            (RegExConcat (RegExBase "a",
            RegExConcat (RegExUserDefined "T", RegExBase "p"))),
          RegExBase "q")))))))))
    (expand_atom_rewrite empty_or_not_star_expansion
      (to_dnf_regex (RegExConcat (RegExConcat (RegExBase "x",RegExUserDefined
      "Z"), RegExConcat (RegExBase "m", RegExConcat (RegExStar (RegExConcat
      (RegExBase "a", RegExConcat (RegExUserDefined "T",RegExBase "p"))),RegExBase
      "q"))))) 0)

let test_expand_atom_empty_internal_union _ =
  test_dnf_lens
    (to_dnf_regex
      (RegExOr
        (RegExOr
          (RegExBase ""
          ,RegExConcat
            (RegExBase "a"
            ,RegExStar (RegExOr (RegExBase "a", RegExBase "b"))))
        ,RegExConcat
          (RegExBase "b"
          ,RegExStar (RegExOr (RegExBase "a",RegExBase "b"))))))
    (expand_atom_rewrite empty_or_not_star_expansion
      (to_dnf_regex
        (RegExStar (RegExOr (RegExBase "a", RegExBase "b")))) 0)

let test_expand_atom_empty_union_first _ =
  test_dnf_lens
    (to_dnf_regex
      (RegExOr
        (RegExOr
          (RegExBase ""
          ,RegExConcat
            (RegExBase "a"
            ,RegExStar (RegExBase "a")))
        ,RegExStar (RegExBase "b"))))
    (expand_atom_rewrite empty_or_not_star_expansion
      (to_dnf_regex
        (RegExOr
          (RegExStar (RegExBase "a")
          ,RegExStar (RegExBase "b")))) 0)

let test_expand_atom_empty_union_second _ =
  test_dnf_lens
    (to_dnf_regex
      (RegExOr
        (RegExOr
          (RegExStar (RegExBase "a")
          ,RegExBase "")
        ,RegExConcat
          (RegExBase "b"
          ,RegExStar (RegExBase "b")))))
    (expand_atom_rewrite empty_or_not_star_expansion
      (to_dnf_regex
        (RegExOr
          (RegExStar (RegExBase "a")
          ,RegExStar (RegExBase "b")))) 1)

let test_expand_atom_quotient _ =
  test_dnf_lens
    (to_dnf_regex
      (RegExOr
        (RegExStar (RegExBase "aa")
        ,RegExConcat
          (RegExBase "a"
          ,RegExStar (RegExBase "aa")))))
    (expand_atom_rewrite
      (quotient_product_expansion 2)
      (to_dnf_regex (RegExStar (RegExBase "a")))
      0)

let expand_atom_rewrite_suite = "expand_atom_rewrite Unit Tests" >:::
  ["test_expand_atom_empty_basic" >:: test_expand_atom_empty_basic;
   "test_expand_atom_empty_concat" >:: test_expand_atom_empty_concat;
   "test_expand_atom_empty_concat_harder" >:: test_expand_atom_empty_concat_harder;
   "test_expand_atom_empty_internal_union" >:: test_expand_atom_empty_internal_union;
   "test_expand_atom_empty_union_first" >:: test_expand_atom_empty_union_first;
   "test_expand_atom_empty_union_second" >:: test_expand_atom_empty_union_second;
   "test_expand_atom_quotient" >:: test_expand_atom_quotient;
  ]

let _ = run_test_tt_main expand_atom_rewrite_suite

let test_dnf_lens_option (expected:dnf_lens option) (actual:dnf_lens option) =
  assert_equal
    ~printer:(fun l -> begin match l with
    | None -> "None"
    | Some l -> "Some " ^ Pp.pp_dnf_lens l end)
    expected
    actual

let test_gen_dnf_lens_const_nosoln _ =
  test_dnf_lens_option
    None
    (gen_dnf_lens []
      (to_dnf_regex (RegExBase "x"))
      (to_dnf_regex (RegExBase "y"))
      [("a","b")])

let test_gen_dnf_lens_const_soln _ =
  test_dnf_lens_option
    (Some ([[],Permutation.create [], ["x"], ["y"]],Permutation.create [0]))
    (gen_dnf_lens []
      (to_dnf_regex (RegExBase "x"))
      (to_dnf_regex (RegExBase "y"))
      [("x","y")])

let test_gen_lenses_union _ =
  test_dnf_lens_option
    (Some
      ([[],Permutation.create [], ["a"], ["y"];
        [],Permutation.create [], ["b"], ["x"]],
      Permutation.create [1;0]))
    (gen_dnf_lens []
      (to_dnf_regex (RegExOr (RegExBase "a", RegExBase "b")))
      (to_dnf_regex (RegExOr (RegExBase "x", RegExBase "y")))
      [("a","y");("b","x")])

let test_gen_lenses_three_union _ =
  test_dnf_lens_option
    (Some
      ([[],Permutation.create [], ["a"], ["y"];
        [],Permutation.create [], ["b"], ["z"];
        [],Permutation.create [], ["c"], ["x"]],
      Permutation.create [2;0;1]))
    (gen_dnf_lens []
      (to_dnf_regex (RegExOr (RegExBase "a", RegExOr (RegExBase "b", RegExBase "c"))))
      (to_dnf_regex (RegExOr (RegExBase "x", RegExOr (RegExBase "y", RegExBase "z"))))
      [("a","y");("b","z");("c","x")])

let test_gen_lenses_userdef_ident _ =
  test_dnf_lens_option
    (Some
      ([[AIdentity], Permutation.create [0], ["";""], ["";""]],
      Permutation.create [0]))
    (gen_dnf_lens ["A",RegExBase "a"; "B", RegExBase "b"]
      (to_dnf_regex (RegExUserDefined "A"))
      (to_dnf_regex (RegExUserDefined "A"))
      [])

let test_gen_lenses_concat_userdef _ =
  test_dnf_lens_option
    (Some
      ([[AIdentity; AIdentity], Permutation.create [1;0],
        ["";"";""], ["";"";""]],
      Permutation.create [0]))
    (gen_dnf_lens ["A",RegExBase "a"; "B", RegExBase "b"]
      (to_dnf_regex (RegExConcat (RegExUserDefined "A", RegExUserDefined "B")))
      (to_dnf_regex (RegExConcat (RegExUserDefined "B", RegExUserDefined "A")))
      ["ab","ba"])

let test_gen_lenses_concat_userdef_hard _ =
  test_dnf_lens_option
    (Some
      ([[AIdentity; AIdentity], Permutation.create [1;0],
        ["";"";""], ["";"";""]],
      Permutation.create [0]))
    (gen_dnf_lens ["A",RegExOr (RegExBase "a", RegExBase "A")]
      (to_dnf_regex (RegExConcat (RegExUserDefined "A", RegExUserDefined "A")))
      (to_dnf_regex (RegExConcat (RegExUserDefined "A", RegExUserDefined "A")))
      [("Aa","aA")])

let test_gen_lenses_star _ =
  test_dnf_lens_option
  (Some ([[(AIterate ([[], Permutation.create [], ["a"], ["b"]], Permutation.create
  [0]))], Permutation.create [0], ["";""], ["";""]], Permutation.create [0]))
    (gen_dnf_lens []
      (to_dnf_regex (RegExStar (RegExBase "a")))
      (to_dnf_regex (RegExStar (RegExBase "b")))
      ["aa","bb"])

let test_gen_dnf_lens_star_difficult _ =
  test_dnf_lens_option
  (Some ([
    [(AIterate ([[], Permutation.create [], ["a"], ["b"]], Permutation.create
    [0]));
    (AIterate ([[], Permutation.create [], ["b"], ["a"]], Permutation.create
    [0]))
    ], Permutation.create [1;0], ["";"";""], ["";"";""]
    ],
    Permutation.create [0]))
    (gen_dnf_lens []
      (to_dnf_regex (RegExConcat
        (RegExStar (RegExBase "a"),
        RegExStar (RegExBase "b"))))
      (to_dnf_regex (RegExConcat
        (RegExStar (RegExBase "a"),
        RegExStar (RegExBase "b"))))
      ["abb","aab"])

let test_dnf_lens_star_expansion _ =
  test_dnf_lens_option
  (Some ([
    [],Permutation.create [], [""], [""];
    [(AIterate ([[], Permutation.create [], ["a"], ["a"]], Permutation.create
    [0]))], Permutation.create [0], ["a";""], ["a";""]
    ],
    Permutation.create [0;1]))
    (gen_dnf_lens []
      (to_dnf_regex (RegExStar (RegExBase "a")))
      (to_dnf_regex (RegExOr
        (RegExBase "",
        RegExConcat (RegExBase "a", RegExStar (RegExBase "a")))))
      ["a","a"])

let test_dnf_lens_star_inner_expansion _ =
  test_dnf_lens_option
    ( Some ([
      [AIterate ([
        ([],Permutation.create [], ["a"], ["a"]);
        ([AIterate ([
          ([],Permutation.create [], ["z"], ["z"])
        ],Permutation.create [0])],Permutation.create [0], ["az";""], ["az";""])
      ],Permutation.create [0;1])],Permutation.create [0], ["";""],
      ["";""]
      ],Permutation.create [0]))
    (gen_dnf_lens []
      (to_dnf_regex (RegExStar (RegExConcat (RegExBase "a", RegExStar (RegExBase
      "z")))))
      (to_dnf_regex (RegExStar (RegExOr (RegExBase "a", RegExConcat (RegExBase
      "az", RegExStar (RegExBase "z"))))))
      [])

let test_dnf_lens_quotient_expansion _ =
  test_dnf_lens_option
    (Some (
      [
        ([
          (AIterate ([[],Permutation.create [], ["aa"],["aa"]], Permutation.create [0]))
        ],
        Permutation.create [0],
        ["";""],
        ["";""]);
        ([
          (AIterate ([[],Permutation.create [], ["aa"],["aa"]], Permutation.create [0]))
        ],
        Permutation.create [0],
        ["a";""],
        ["a";""]);
      ], Permutation.create [0;1]))
    (gen_dnf_lens []
      (to_dnf_regex (RegExStar (RegExBase "a")))
      (to_dnf_regex (RegExOr (RegExStar (RegExBase "aa"),
                            RegExConcat (RegExBase "a", RegExStar (RegExBase
                            "aa")))))
      [])

let test_dnf_lens_inner_quotient_expansion _ =
  test_dnf_lens_option
  (Some (
    [
      ([
        AIterate 
(
      [
        ([
          (AIterate ([[],Permutation.create [], ["bb"],["bb"]], Permutation.create [0]))
        ],
        Permutation.create [0],
        ["a";""],
        ["a";""]);
        ([
          (AIterate ([[],Permutation.create [], ["bb"],["bb"]], Permutation.create [0]))
        ],
        Permutation.create [0],
        ["ab";""],
        ["ab";""]);
      ], Permutation.create [1;0])
    ], Permutation.create [0], ["";""], ["";""])], Permutation.create [0]))
    (gen_dnf_lens []
      (to_dnf_regex (RegExStar (RegExConcat (RegExBase "a", RegExStar (RegExBase
    "b")))))
      (to_dnf_regex (RegExStar (RegExConcat (RegExBase "a", RegExOr (RegExStar (RegExBase "bb"),
                            RegExConcat (RegExBase "b", RegExStar (RegExBase
                            "bb")))))))
      [("a","a")])

let gen_dnf_lens_suite = "gen_dnf_lens Unit Tests" >:::
  ["test_gen_dnf_lens_const_nosoln" >:: test_gen_dnf_lens_const_nosoln;
   "test_gen_dnf_lens_const_soln" >:: test_gen_dnf_lens_const_soln;
   "test_gen_lenses_union" >:: test_gen_lenses_union;
   "test_gen_lenses_three_union" >:: test_gen_lenses_three_union;
   "test_gen_lenses_userdef_ident" >:: test_gen_lenses_userdef_ident;
   "test_gen_lenses_concat_userdef" >:: test_gen_lenses_concat_userdef;
   "test_gen_lenses_concat_userdef_hard" >:: test_gen_lenses_concat_userdef_hard;
   "test_gen_lenses_star" >:: test_gen_lenses_star;
   "test_gen_dnf_lens_star_difficult" >:: test_gen_dnf_lens_star_difficult;
   "test_dnf_lens_star_expansion" >:: test_dnf_lens_star_expansion;
   "test_dnf_lens_star_inner_expansion" >:: test_dnf_lens_star_inner_expansion;
   "test_dnf_lens_quotient_expansion" >:: test_dnf_lens_quotient_expansion;
   "test_dnf_lens_inner_quotient_expansion" >:: test_dnf_lens_inner_quotient_expansion;
  ]

let _ = run_test_tt_main gen_dnf_lens_suite


let test_to_exampled_dnf_constant_noex _ =
  test_exampled_dnf_option
    (Some [([],["a"],[])])
    (to_exampled_dnf_regex []
      [([],["a"])]
      [])

let test_to_exampled_dnf_constant_2ex _ =
  test_exampled_dnf_option
    (Some [([],["a"],[1;0])])
    (to_exampled_dnf_regex []
      [([],["a"])]
      ["a";"a"])

let test_to_exampled_dnf_or _ =
  test_exampled_dnf_option
    (Some [([],["a"],[1]);([],["b"],[0])])
    (to_exampled_dnf_regex []
      [([],["a"]);([],["b"])]
      ["b";"a"])

let test_to_exampled_dnf_userdefined _ =
  test_exampled_dnf_option
    (Some [([EAUserDefined ("A",["a"])],["";""],[0])])
    (to_exampled_dnf_regex ["A",RegExBase "a"]
      [([AUserDefined "A"],["";""])]
      ["a"])

let test_to_exampled_dnf_star _ =
  test_exampled_dnf_option
    (Some [([EAStar [[],["a"],[1;0]]],["";""],[0])])
    (to_exampled_dnf_regex []
      [([AStar [([],["a"])]],["";""])]
      ["aa"])


let test_to_exampled_dnf_suite = "to_exampled_dnf_regex Unit Tests" >:::
  ["test_to_exampled_dnf_constant_noex" >:: test_to_exampled_dnf_constant_noex;
   "test_to_exampled_dnf_constant_2ex" >:: test_to_exampled_dnf_constant_2ex;
   "test_to_exampled_dnf_or" >:: test_to_exampled_dnf_or;
   "test_to_exampled_dnf_userdefined" >:: test_to_exampled_dnf_userdefined;
   "test_to_exampled_dnf_star" >:: test_to_exampled_dnf_star;
  ]

let _ = run_test_tt_main test_to_exampled_dnf_suite
