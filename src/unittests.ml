open Core.Std
open Counters
open Priority_queue
open Disjointset
open Regexcontext
open Lenscontext
open Fasteval
open Permutation
open Ounit_extensions
open Ounit_general_extensions
open Util
open OUnit2
open Eval
open Lens
open Lang
open Pp
open Gen
open Transform
open Lens_put

let test_to_normalized_exp_base _ =
  assert_dnf_equal
    (to_dnf_regex (RegExBase "x"))
    [([],["x"])]

let test_to_normalized_exp_concat_easy _ =
  assert_dnf_equal
    [([],["ab"])]
    (to_dnf_regex
      (RegExConcat
        (RegExBase "a",
        RegExBase "b")))
    
let test_to_normalized_exp_concat_tree _ =
  assert_dnf_equal
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
  assert_dnf_equal
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
  assert_dnf_equal
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
  assert_dnf_equal
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



let test_counters_add_same _ =
  assert_ordered_string_assoc_list_equal
    [("a",2.0)]
    (Counters.as_ordered_assoc_list (Counters.add (Counters.add (Counters.create
    comparison_compare) "a") "a"))

let test_counters_add_different _ =
  assert_ordered_string_assoc_list_equal
    [("a",1.0);("b",1.0)]
    (Counters.as_ordered_assoc_list (Counters.add (Counters.add (Counters.create
    comparison_compare) "a") "b"))

let test_counters_add_different_rev _ =
  assert_ordered_string_assoc_list_equal
    [("a",1.0);("b",1.0)]
    (Counters.as_ordered_assoc_list (Counters.add (Counters.add (Counters.create
    comparison_compare) "b") "a"))

let test_counters_merge_same _ =
  assert_ordered_string_assoc_list_equal
    [("a",2.0)]
    (Counters.as_ordered_assoc_list ((Counters.merge (fun x y -> x+.y) (Counters.add (Counters.create comparison_compare) "a")
    (Counters.add (Counters.create comparison_compare) "a"))))

let test_counters_merge_different _ =
  assert_ordered_string_assoc_list_equal
    [("a",1.0);("b",1.0)]
    (Counters.as_ordered_assoc_list ((Counters.merge (fun x y -> x+.y) (Counters.add (Counters.create comparison_compare) "a")
    (Counters.add (Counters.create comparison_compare) "b"))))

let test_counters_merge_different_rev _ =
  assert_ordered_string_assoc_list_equal
    [("a",1.0);("b",1.0)]
    (Counters.as_ordered_assoc_list ((Counters.merge (fun x y -> x+.y) (Counters.add (Counters.create comparison_compare) "b")
    (Counters.add (Counters.create comparison_compare) "a"))))

let counters_suite = "compare_dnf_regexs Unit Tests" >:::
  ["test_counters_add_same" >:: test_counters_add_same;
   "test_counters_add_different" >:: test_counters_add_different;
   "test_counters_add_different_rev" >:: test_counters_add_different_rev;
   "test_counters_merge_same" >:: test_counters_merge_same;
   "test_counters_merge_different" >:: test_counters_merge_different;
   "test_counters_merge_different_rev" >:: test_counters_merge_different_rev;
  ]

let _ = run_test_tt_main counters_suite


let empty_int : int DisjointSet.t = DisjointSet.empty (=)
let ds_1234equal : int DisjointSet.t =
  DisjointSet.create_from_equivalences (=)
    [(1,2);(3,4);(2,3)]

let test_disjointset_singleton _ =
  assert_int_equal
    1
    (DisjointSet.find_representative empty_int 1)

let test_disjointset_setequiv _ =
  assert_int_equal
    (DisjointSet.find_representative ds_1234equal 1)
    (DisjointSet.find_representative ds_1234equal 4)

let test_disjointset_disjoint _ =
  assert_not_equal_int
    (DisjointSet.find_representative ds_1234equal 0)
    (DisjointSet.find_representative ds_1234equal 2)

let disjointset_suite = "DisjointSet Unit Tests" >:::
  [
    "test_disjointset_singleton" >:: test_disjointset_singleton;
    "test_disjointset_setequiv" >:: test_disjointset_setequiv;
    "test_disjointset_disjoint" >:: test_disjointset_disjoint;
  ]

let _ = run_test_tt_main disjointset_suite


let test_to_exampled_dnf_constant_noex _ =
  assert_exampled_dnf_option_equal
    (Some ([([],["a"],[])],[]))
    (regex_to_exampled_dnf_regex RegexContext.empty []
      (RegExBase "a")
      [])

let test_to_exampled_dnf_constant_2ex _ =
  assert_exampled_dnf_option_equal
    (Some ([([],["a"],[[1];[0]])],[[1];[0]]))
    (regex_to_exampled_dnf_regex RegexContext.empty []
      (RegExBase "a")
      ["a";"a"])

let test_to_exampled_dnf_or _ =
  assert_exampled_dnf_option_equal
    (Some ([([],["a"],[[1]]);([],["b"],[[0]])],[[1];[0]]))
    (regex_to_exampled_dnf_regex RegexContext.empty []
      (RegExOr (RegExBase "a", RegExBase "b"))
      ["b";"a"])

let test_to_exampled_dnf_userdefined _ =
  assert_exampled_dnf_option_equal
    (Some ([([EAUserDefined ("A",["a"],[[0]])],["";""],[[0]])],[[0]]))
    (regex_to_exampled_dnf_regex (RegexContext.create_from_list_exn ["A",RegExBase "a",true]) []
      (RegExUserDefined "A")
      ["a"])

let test_to_exampled_dnf_star _ =
  assert_exampled_dnf_option_equal
  (Some ([([EAStar (([[],["a"],[[1;0];[0;0]]],[[1;0];[0;0]]),[[0]])],["";""],[[0]])],[[0]]))
    (regex_to_exampled_dnf_regex RegexContext.empty []
      (RegExStar (RegExBase "a"))
      ["aa"])

(*let test_to_exampled_dnf_star_udef_or_concat _ =
  assert_exampled_dnf_option_equal
    (Some ([],[[0]]))
    (regex_to_exampled_dnf_regex ["A",RegExBase "a"]
      (RegExConcat
        ((RegExStar (RegExUserDefined "A"))
        ,(RegExOr (RegExBase "c",RegExBase "d"))))
      ["aac"])*)


let test_to_exampled_dnf_suite = "to_exampled_dnf_regex Unit Tests" >:::
  ["test_to_exampled_dnf_constant_noex" >:: test_to_exampled_dnf_constant_noex;
   "test_to_exampled_dnf_constant_2ex" >:: test_to_exampled_dnf_constant_2ex;
   "test_to_exampled_dnf_or" >:: test_to_exampled_dnf_or;
   "test_to_exampled_dnf_userdefined" >:: test_to_exampled_dnf_userdefined;
   "test_to_exampled_dnf_star" >:: test_to_exampled_dnf_star;
   (*"test_to_exampled_dnf_star_udef_or_concat" >::
     * test_to_exampled_dnf_star_udef_or_concat;*)
  ]

let _ = run_test_tt_main test_to_exampled_dnf_suite

let test_compare_dnf_regexs_userdefineds_eq _ =
  assert_comparison_equal
    EQ
    (compare_dnf_regexs [[AUserDefined "a"],["";"1qaz"]]
    [[AUserDefined "a"],["";"2wsx"]])

let test_compare_dnf_regexs_userdefineds_lt _ =
  assert_comparison_equal
    LT
    (compare_dnf_regexs [[AUserDefined "a"],["";"1qaz"]]
    [[AUserDefined "b"],["";"2wsx"]])

let compare_dnf_regexs_suite = "compare_dnf_regexs Unit Tests" >:::
  ["test_compare_dnf_regexs_userdefineds_eq" >:: test_compare_dnf_regexs_userdefineds_eq;
   "test_compare_dnf_regexs_userdefineds_lt" >:: test_compare_dnf_regexs_userdefineds_lt;
  ]

let _ = run_test_tt_main compare_dnf_regexs_suite

let test_compare_exampled_dnf_regexs_userdefineds_eq _ =
  assert_comparison_equal
    EQ
    (compare_exampled_dnf_regexs ([[EAUserDefined
    ("a",["a";"aa"],[[0];[1]])],["";"1qaz"],[[0];[1]]],[[0];[1]])
    ([[EAUserDefined
    ("a",["a";"aa"],[[0];[1]])],["";"2wsx"],[[0];[1]]],[[0];[1]]))

let test_compare_exampled_dnf_regexs_userdefineds_lt1 _ =
  assert_comparison_equal
    LT
    (compare_exampled_dnf_regexs ([[EAUserDefined
    ("a",["b"],[[0]])],["";"1qaz"],[[0]]],[[0]])
    ([[EAUserDefined ("b",["a"],[[0]])],["";"2wsx"],[[0]]],[[0]]))

let test_compare_exampled_dnf_regexs_userdefineds_lt2 _ =
  assert_comparison_equal
    LT
    (compare_exampled_dnf_regexs ([[EAUserDefined
    ("a",["a"],[[0]])],["";"1qaz"],[[0]]],[[0]])
    ([[EAUserDefined ("a",["b"],[[0]])],["";"2wsx"],[[0]]],[[0]]))

let compare_equivalent_dnf_regexs_suite = "compare_dnf_regexs Unit Tests" >:::
  ["test_compare_exampled_dnf_regexs_userdefineds_eq" >:: test_compare_exampled_dnf_regexs_userdefineds_eq;
   "test_compare_exampled_dnf_regexs_userdefineds_lt1" >:: test_compare_exampled_dnf_regexs_userdefineds_lt1;
   "test_compare_exampled_dnf_regexs_userdefineds_lt2" >:: test_compare_exampled_dnf_regexs_userdefineds_lt2;
  ]

let _ = run_test_tt_main compare_equivalent_dnf_regexs_suite




let test_rxc_concrete_name = "concrete_name"
let test_rxc_concrete_base = (RegExBase "concrete_base")
let test_rxc_abstract_name = "abstract_name"
let test_rxc_abstract_base = (RegExBase "abstract_base")
let test_rxc_context =
  RegexContext.create_from_list_exn
    [(test_rxc_concrete_name,test_rxc_concrete_base,false)
    ;(test_rxc_abstract_name,test_rxc_abstract_base,true)]

let test_lookup_empty _ =
  assert_raises
    (Failure "find_exn: key not found")
    (fun _ -> RegexContext.lookup_exn RegexContext.empty "none")

let test_lookup_abstract _ =
  assert_regex_equal
    test_rxc_abstract_base
    (RegexContext.lookup_exn test_rxc_context test_rxc_abstract_name)

let test_lookup_concrete _ =
  assert_regex_equal
    test_rxc_concrete_base
    (RegexContext.lookup_exn test_rxc_context test_rxc_concrete_name)

let test_insert_same =
  RegexContext.insert_exn
    test_rxc_context
    test_rxc_abstract_name
    test_rxc_abstract_base
    true

let test_insert_conflicted _ =
  assert_raises
    (Failure "bad insert")
    (fun _ ->
       RegexContext.insert_exn
         test_rxc_context
         test_rxc_concrete_name
         test_rxc_abstract_base
         false)

let test_lookup_for_expansion_empty _ =
  assert_raises
    (Failure "bad regex name")
    (fun _ -> RegexContext.lookup_for_expansion_exn RegexContext.empty "none")

(* TODO: tests on generated ids *)

let regex_context_suite = "RegexContext Unit Tests" >:::
  [
    "test_lookup_empty" >:: test_lookup_empty;
    "test_lookup_abstract" >:: test_lookup_abstract;
    "test_lookup_concrete" >:: test_lookup_concrete;
    "test_insert_conflicted" >:: test_insert_conflicted;
    "test_lookup_for_expansion_empty" >:: test_lookup_for_expansion_empty;
  ]

let _ = run_test_tt_main regex_context_suite

let test_lc_name = "my_lens"
let test_lc_regex_sname = "userdef_source"
let test_lc_regex_tname = "userdef_target"
let test_lc_regex_source = RegExUserDefined test_lc_regex_sname
let test_lc_regex_target = RegExUserDefined test_lc_regex_tname
let test_lc_lens = ConstLens ("a","b")
let test_lc_context =
  LensContext.create_from_list_exn
    [(test_lc_name,test_lc_lens,test_lc_regex_source,test_lc_regex_target)]

let test_lc_lookup_empty _ =
  assert_raises
    (Failure "find_exn: key not found")
    (fun _ -> LensContext.lookup_exn LensContext.empty "none")

let test_lc_lookup_type_empty _ =
  assert_raises
    (Failure "find_exn: key not found")
    (fun _ -> LensContext.lookup_type_exn LensContext.empty "none")

let test_lc_lookup_impl_empty _ =
  assert_raises
    (Failure "find_exn: key not found")
    (fun _ -> LensContext.lookup_impl_exn LensContext.empty "none")

let test_lc_lookup _ =
  assert_lens_regex_regex_equal
    (test_lc_lens, test_lc_regex_source, test_lc_regex_target)
    (LensContext.lookup_exn test_lc_context test_lc_name)

let test_lc_lookup_type _ =
  assert_regex_regex_equal
    (test_lc_regex_source, test_lc_regex_target)
    (LensContext.lookup_type_exn test_lc_context test_lc_name)

let test_lc_lookup_impl _ =
  assert_lens_equal
    (test_lc_lens)
    (LensContext.lookup_impl_exn test_lc_context test_lc_name)

let test_lc_insert_conflicted _ =
  assert_raises
    (Failure "bad insert")
    (fun _ ->
       LensContext.insert_exn
         test_lc_context
         test_lc_name
         test_lc_lens
         test_lc_regex_source
         test_lc_regex_target)

let test_lc_paths_to_rep_elt_1 _ =
  assert_id_lens_list_equal
    (test_lc_regex_tname,[IdentityLens (test_lc_regex_target)])
    (LensContext.paths_to_rep_elt
       test_lc_context
       test_lc_regex_tname)

let test_lc_paths_to_rep_elt_2 _ =
  assert_id_lens_list_equal
    (test_lc_regex_tname,
     [ComposeLens (test_lc_lens,IdentityLens (test_lc_regex_source))])
    (LensContext.paths_to_rep_elt
       test_lc_context
       test_lc_regex_sname)

let test_lc_paths_to_rep_elt_singleton _ =
  assert_id_lens_list_equal
    ("sinlgetonname",
     [IdentityLens (RegExUserDefined "singletonname")])
    (LensContext.paths_to_rep_elt
       test_lc_context
       "sinlgetonname")

let lens_context_suite = "LensContext Unit Tests" >:::
  [
    "test_lc_lookup_empty" >:: test_lc_lookup_empty;
    "test_lc_lookup_type_empty" >:: test_lc_lookup_type_empty;
    "test_lc_lookup_impl_empty" >:: test_lc_lookup_impl_empty;
    "test_lc_lookup" >:: test_lc_lookup;
    "test_lc_lookup_type" >:: test_lc_lookup_type;
    "test_lc_lookup_impl" >:: test_lc_lookup_impl;
    "test_lc_insert_conflicted" >:: test_lc_insert_conflicted;
    "test_lc_paths_to_rep_elt_1" >:: test_lc_paths_to_rep_elt_1;
    "test_lc_paths_to_rep_elt_2" >:: test_lc_paths_to_rep_elt_2;
  ]

let _ = run_test_tt_main lens_context_suite





(* Eval tests *)
let test_fast_eval_base_positive _ =
  assert_bool_equal
    true
    (fast_eval RegexContext.empty [] (RegExBase "x") "x")

let test_fast_eval_base_negative1 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty [] (RegExBase "x") "y")

let test_fast_eval_base_negative2 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty [] (RegExBase "x") "xx")

let test_fast_eval_concat_positive1 _ =
  assert_bool_equal
    true
    (fast_eval RegexContext.empty []
      (RegExConcat
        (RegExBase "x",
        RegExConcat
          (RegExBase "y",
          RegExBase "z"))) "xyz")

let test_fast_eval_concat_positive2 _ =
  assert_bool_equal
    true
    (fast_eval RegexContext.empty []
      (RegExConcat
        (RegExConcat
          (RegExBase "x",
          RegExBase "y"),
        RegExBase "z")) "xyz")

let test_fast_eval_concat_negative1 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty []
      (RegExConcat
        (RegExBase "x",
        RegExBase "y")) "x")

let test_fast_eval_concat_negative2 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty []
      (RegExConcat
        (RegExBase "x",
        RegExBase "y")) "xz")

let test_fast_eval_concat_negative3 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty []
      (RegExConcat
        (RegExBase "x",
        RegExBase "y")) "yx")

let test_fast_eval_concat_negative4 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty []
      (RegExConcat
        (RegExBase "x",
        RegExBase "y")) "xyz")

let test_fast_eval_or_positive _ =
  assert_bool_equal
    true
    (fast_eval RegexContext.empty []
      (RegExOr
        (RegExOr
          (RegExBase "a",
          RegExBase "b"),
        (RegExOr
          (RegExBase "c",
          RegExBase "d")))) "c")

let test_fast_eval_or_negative _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty []
      (RegExOr
        (RegExOr
          (RegExBase "a",
          RegExBase "b"),
        (RegExOr
          (RegExBase "c",
          RegExBase "d")))) "x")

let test_fast_eval_star_empty _ =
  assert_bool_equal
    true
    (fast_eval RegexContext.empty []
      (RegExStar
        (RegExBase "a")) "")

let test_fast_eval_star_one _ =
  assert_bool_equal
    true
    (fast_eval RegexContext.empty []
      (RegExStar
        (RegExBase "a")) "a")

let test_fast_eval_star_two _ =
  assert_bool_equal
    true
    (fast_eval RegexContext.empty []
      (RegExStar
        (RegExBase "a")) "aa")

let test_fast_eval_star_choice _ =
  assert_bool_equal
    true
    (fast_eval RegexContext.empty []
      (RegExStar
        (RegExOr
          (RegExBase "a"
          ,RegExBase "b"))
      ) "aab" )

let test_fast_eval_star_negative1 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty []
      (RegExStar
        (RegExBase "a")) "b")

let test_fast_eval_star_negative2 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty []
      (RegExStar
        (RegExBase "a")) "ab")

let test_fast_eval_star_negative3 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty []
      (RegExStar
        (RegExBase "a")) "ba")

let test_fast_eval_userdef_positive _ =
  assert_bool_equal
    true
    (fast_eval (RegexContext.create_from_list_exn ["A",RegExBase "a",true]) []
      (RegExUserDefined "A") "a")

let test_fast_eval_userdef_negative _ =
  assert_bool_equal
    false
    (fast_eval (RegexContext.create_from_list_exn ["A",RegExBase "a",true]) []
      (RegExUserDefined "A") "b")

let test_fast_eval_concat_userdef _ =
  assert_bool_equal
  true
  (fast_eval
    (RegexContext.create_from_list_exn [("A", RegExBase "a",true);("B", RegExBase "b",true)])
    []
    (RegExConcat (RegExUserDefined "A", RegExUserDefined "B"))
    "ab")

let test_fast_eval_nested_userdef _ =
  assert_bool_equal
  true
  (fast_eval
   (RegexContext.create_from_list_exn [("A", RegExBase "a", true);("B", RegExUserDefined "A",true)]) []
   (RegExUserDefined "B") "a")

let test_fast_eval_fast _ =
  assert_bool_equal
  true
  (fast_eval
     (RegexContext.create_from_list_exn [("A", RegExConcat (RegExBase "c", RegExConcat (RegExStar
  (RegExBase "a"), RegExStar (RegExBase "b"))),true)]) []
  (RegExConcat (RegExStar (RegExUserDefined "A"), RegExConcat (RegExBase "z",
  RegExStar (RegExConcat (RegExConcat (RegExUserDefined "A", RegExBase "q"),
  RegExStar (RegExConcat (RegExUserDefined "A", RegExOr (RegExBase "t",
  RegExBase "m"))))))))
  "caaabbbbcaaabbbbcaaabbbbcaaabbbbcaaabbbbcaaabbbbcaaabbbbcaaabbbbzcaaabbbqcaaabbbtcaaabbbqcaaabbbtcaaabbbqcaaabbbtcaaabbbqcaaabbbtcaaabbbqcaaabbbtcaaabbbqcaaabbbtcaaabbbqcaaabbbtcaaabbbqcaaabbbtcaaabbbqcaaabbbtcaaabbbqcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbmcaaabbbm")

let fast_eval_suite = "fast_eval Unit Tests" >:::
  ["test_fast_eval_base_positive" >:: test_fast_eval_base_positive;
   "test_fast_eval_base_negative1" >:: test_fast_eval_base_negative1;
   "test_fast_eval_base_negative2" >:: test_fast_eval_base_negative2;
   "test_fast_eval_concat_positive1" >:: test_fast_eval_concat_positive1;
   "test_fast_eval_concat_positive2" >:: test_fast_eval_concat_positive2;
   "test_fast_eval_concat_negative1" >:: test_fast_eval_concat_negative1;
   "test_fast_eval_concat_negative2" >:: test_fast_eval_concat_negative2;
   "test_fast_eval_concat_negative3" >:: test_fast_eval_concat_negative3;
   "test_fast_eval_concat_negative4" >:: test_fast_eval_concat_negative4;
   "test_fast_eval_or_negative" >:: test_fast_eval_or_negative;
   "test_fast_eval_or_positive" >:: test_fast_eval_or_positive;
   "test_fast_eval_star_empty" >:: test_fast_eval_star_empty;
   "test_fast_eval_star_one" >:: test_fast_eval_star_one;
   "test_fast_eval_star_two" >:: test_fast_eval_star_two;
   "test_fast_eval_star_choice" >:: test_fast_eval_star_choice;
   "test_fast_eval_star_negative1" >:: test_fast_eval_star_negative1;
   "test_fast_eval_star_negative2" >:: test_fast_eval_star_negative2;
   "test_fast_eval_star_negative3" >:: test_fast_eval_star_negative3;
   "test_fast_eval_userdef_positive" >:: test_fast_eval_userdef_positive;
   "test_fast_eval_userdef_negative" >:: test_fast_eval_userdef_negative;
   "test_fast_eval_concat_userdef" >:: test_fast_eval_concat_userdef;
   "test_fast_eval_nested_userdef" >:: test_fast_eval_nested_userdef;
   "test_fast_eval_fast" >:: test_fast_eval_fast;
  ]

let _ = run_test_tt_main fast_eval_suite

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
  assert_string_double_option_equal
    (Some ("a","b"))
    (retrieve_regex_concat_split RegexContext.empty (RegExBase "a") (RegExBase "b") "ab")

let retrieve_regex_concat_split_suite = "retrieve_regex_concat_split Unit Tests" >:::
  ["test_retrieve_regex_concat_split" >:: test_retrieve_regex_concat_split;
  ]

let _ = run_test_tt_main retrieve_regex_concat_split_suite

let test_retrieve_regex_star_splits _ =
  assert_string_list_option_equal
    (Some ["a";"a"])
    (retrieve_regex_star_splits RegexContext.empty (RegExBase "a") "aa")

let retrieve_regex_star_splits_suite = "retrieve_regex_star_splits Unit Tests" >:::
  ["test_retrieve_regex_star_splits" >:: test_retrieve_regex_star_splits;
  ]

let _ = run_test_tt_main retrieve_regex_star_splits_suite

let test_retrieve_dnf_clause_choices _ =
  assert_int_option_equal
    (Some 1)
    (retrieve_dnf_clause_choices RegexContext.empty [([],["a"]);([],["b"])] "b")

let retrieve_dnf_clause_choices_suite = "retrieve_dnf_clause_choices Unit Tests" >:::
  ["test_retrieve_dnf_clause_choices" >:: test_retrieve_dnf_clause_choices;
  ]

let _ = run_test_tt_main retrieve_dnf_clause_choices_suite

let test_retrieve_atom_splits _ =
  assert_string_list_option_equal
    (Some ["a";"b"])
    (retrieve_atom_splits (RegexContext.create_from_list_exn [("A",RegExBase "a",true);("B",RegExBase "b",true)])
    ([AUserDefined "A"; AUserDefined "B"],["qwer";"asdf";"zxcv"])
    "qweraasdfbzxcv")

let test_retrieve_atom_splits_star _ =
  assert_string_list_option_equal
    (Some [""])
    (retrieve_atom_splits RegexContext.empty ([AStar [([],["a"])]],["a";""]) "a")

let retrieve_atom_splits_suite = "retrieve_atom_splits Unit Tests" >:::
  ["test_retrieve_atom_splits" >:: test_retrieve_atom_splits;
   "test_retrieve_atom_splits_star" >:: test_retrieve_atom_splits_star;
  ]

let _ = run_test_tt_main retrieve_atom_splits_suite


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
  assert_permutation_guesses_option_equal
    None
    (Permutation.create_from_constraints 3 [(0,2);(0,1);(0,0)] [])

let test_permutation_create_from_constraints_none_1 _ =
  assert_permutation_guesses_option_equal
    None
    (Permutation.create_from_constraints 3 [(0,0);(1,0);(2,0)] [])

let test_permutation_create_from_constraints_identity _ =
  assert_permutation_guesses_option_equal
    (Some ((Permutation.create [0;1;2]), [(0,0);(1,1);(2,2)]))
    (Permutation.create_from_constraints 3 [] [])

let test_permutation_create_from_constraints_withvalid _ =
  assert_permutation_guesses_option_equal
    (Some ((Permutation.create [1;0;2]), [(1,0);(2,2)]))
    (Permutation.create_from_constraints 3 [] [(0,1)])

let test_permutation_create_from_constraints_withinvalid _ =
  assert_permutation_guesses_option_equal
    (Some ((Permutation.create [1;0;2]), [(0,1);(1,0);(2,2)]))
    (Permutation.create_from_constraints 3 [(0,0)] [])

let test_permutation_apply_identity _ =
  assert_int_equal
     2
     (Permutation.apply (Permutation.create [0;1;2]) 2)

let test_permutation_apply_nonidentity _ =
  assert_int_equal
    1
    (Permutation.apply (Permutation.create [1;2;0]) 2)

let test_permutation_apply_from_doubles _ =
  assert_int_equal
    0
    (Permutation.apply
      (Permutation.create_from_doubles [(1,2);(2,0);(0,1)])
      2)
                
let test_permutation_apply_invalid _ =
  assert_raises
    (Failure "out of range")
    (fun _ -> Permutation.apply (Permutation.create []) 0)

let test_permutation_apply_inverse_identity _ =
  assert_int_equal
    2
    (Permutation.apply_inverse (Permutation.create [0;1;2]) 2)

let test_permutation_apply_inverse_nonidentity _ =
  assert_int_equal
    0
    (Permutation.apply_inverse (Permutation.create [1;2;0]) 2)
                
let test_permutation_apply_inverse_invalid _ =
  assert_raises
    (Failure "out of range")
    (fun _ -> Permutation.apply_inverse (Permutation.create []) 0)

let test_permutation_create_all _ =
  assert_int_equal
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

let test_bucketize_pairs_symmetric _ =
  assert_char_list_list_equal
    [[];['a'];[]]
    (bucketize_pairs 3 [('a',1)])

let test_bucketize_pairs_asymmetric _ =
  assert_char_list_list_equal
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
  assert_char_list_double_equal
    ([],['a'])
    (split_at_index_exn ['a'] 0)

let test_index_split_at_index_exn _ =
  assert_char_list_double_equal
    (['a'],['b'])
    (split_at_index_exn ['a';'b'] 1)

let test_sort_and_partition _ =
  assert_char_list_list_equal
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

let _ = run_test_tt_main util_suite

let test_priority_queue_pop_empty _ =
  assert_int_float_int_priority_queue_option_equal
    None
    (Priority_Queue.pop (Priority_Queue.create))

let test_priority_queue_pop_forward _ =
  assert_int_float_int_priority_queue_option_equal
    (Some (2, 0.5, Priority_Queue.create_from_list [(1,1.0)]))
    (Priority_Queue.pop (Priority_Queue.create_from_list [(1,1.0);(2,0.5)]))

let test_priority_queue_pop_backwards _ =
  assert_int_float_int_priority_queue_option_equal
    (Some (2, 0.5, Priority_Queue.create_from_list [(1,1.0)]))
    (Priority_Queue.pop (Priority_Queue.create_from_list [(2,0.5);(1,1.0)]))

let priority_queue_suite = "Priority_Queue Unit Tests" >:::
  ["test_priority_queue_pop_empty" >:: test_priority_queue_pop_empty;
   "test_priority_queue_pop_forward" >:: test_priority_queue_pop_forward;
   "test_priority_queue_pop_backwards" >:: test_priority_queue_pop_backwards;
  ]

let _ = run_test_tt_main priority_queue_suite

let test_extract_string_base _ =
  assert_string_equal
    "base"
    (extract_string
       (ERegExBase ("base",[[0]]))
       [0])

let test_extract_string_concat _ =
  assert_string_equal
    "concat"
    (extract_string
       (ERegExConcat
          (ERegExBase ("con",[[0]])
          ,ERegExBase ("cat",[[0]])
          ,[[0]]))
       [0])

let test_extract_string_union_left _ =
  assert_string_equal
    "union"
    (extract_string
       (ERegExOr
          (ERegExBase ("union",[[0]])
          ,ERegExBase ("not",[])
          ,[[0]]))
       [0])

let test_extract_string_union_right _ =
  assert_string_equal
    "union"
    (extract_string
       (ERegExOr
          (ERegExBase ("not",[])
          ,ERegExBase ("union",[[0]])
          ,[[0]]))
       [0])

let test_extract_string_iterate _ =
  assert_string_equal
    "iii"
    (extract_string
       (ERegExStar
          (ERegExBase ("i",[[0;0];[1;0];[2;0];[0;1]])
          ,[[0]]))
       [0])

let test_extract_string_userdefined _ =
  assert_string_equal
    "userdefined"
    (extract_string
       (ERegExUserDefined ("t",["userdefined";"not"],[[0];[1]]))
       [0])

let test_extract_string_mappeduserdefined _ =
  assert_string_equal
    "mappeduserdefined"
    (extract_string
       (ERegExMappedUserDefined (13,["mappeduserdefined";"not"],[[0];[1]]))
       [0])

let extract_string_suite = "extract_string Unit Tests" >:::
  [
    "test_extract_string_base" >:: test_extract_string_base;
    "test_extract_string_concat" >:: test_extract_string_concat;
    "test_extract_string_union_left" >:: test_extract_string_union_left;
    "test_extract_string_union_right" >:: test_extract_string_union_right;
    "test_extract_string_iterate" >:: test_extract_string_iterate;
    "test_extract_string_userdefined" >:: test_extract_string_userdefined;
    "test_extract_string_mappeduserdefined" >:: test_extract_string_mappeduserdefined;
  ]

let _ = run_test_tt_main extract_string_suite

let test_lens_putr_const _ =
  assert_string_equal
    "target"
    (lens_putr RegexContext.empty (ConstLens ("source","target")) "source")

let test_lens_putr_concat _ =
  assert_string_equal
    "t1t2"
    (lens_putr
       RegexContext.empty
       (ConcatLens ((ConstLens ("s1","t1")),ConstLens ("s2","t2")))
       "s1s2")

let test_lens_putr_swap _ =
  assert_string_equal
    "t2t1"
    (lens_putr
       RegexContext.empty
       (SwapLens ((ConstLens ("s1","t1")),ConstLens ("s2","t2")))
       "s1s2")

let test_lens_putr_union_left _ =
  assert_string_equal
    "t1"
    (lens_putr
       RegexContext.empty
       (UnionLens ((ConstLens ("s1","t1")),ConstLens ("s2","t2")))
       "s1")

let test_lens_putr_union_right _ =
  assert_string_equal
    "t2"
    (lens_putr
       RegexContext.empty
       (UnionLens ((ConstLens ("s1","t1")),ConstLens ("s2","t2")))
       "s2")

let test_lens_putr_compose _ =
  assert_string_equal
    "u"
    (lens_putr
       RegexContext.empty
       (ComposeLens ((ConstLens ("t","u")),ConstLens ("s","t")))
       "s")

let test_lens_putr_iterate_empty _ =
  assert_string_equal
    ""
    (lens_putr
       RegexContext.empty
       (IterateLens (ConstLens ("s","t")))
       "")

let test_lens_putr_iterate_singleton _ =
  assert_string_equal
    "t"
    (lens_putr
       RegexContext.empty
       (IterateLens (ConstLens ("s","t")))
       "s")

let test_lens_putr_iterate_multiple _ =
  assert_string_equal
    "ttt"
    (lens_putr
       RegexContext.empty
       (IterateLens (ConstLens ("s","t")))
       "sss")

let test_lens_putr_identity _ =
  assert_string_equal
    "source"
    (lens_putr
       RegexContext.empty
       (IdentityLens (RegExBase "source"))
       "source")

let test_inverse_putr _ =
  assert_string_equal
    "source"
    (lens_putr
       RegexContext.empty
       (InverseLens (ConstLens ("source","target")))
       "target")

let lens_putr_suite = "lens_putr Unit Tests" >:::
  [
    "test_lens_putr_const" >:: test_lens_putr_const;
    "test_lens_putr_concat" >:: test_lens_putr_concat;
    "test_lens_putr_swap" >:: test_lens_putr_swap;
    "test_lens_putr_union_left" >:: test_lens_putr_union_left;
    "test_lens_putr_union_right" >:: test_lens_putr_union_right;
    "test_lens_putr_compose" >:: test_lens_putr_compose;
    "test_lens_putr_iterate_empty" >:: test_lens_putr_iterate_empty;
    "test_lens_putr_iterate_singleton" >:: test_lens_putr_iterate_singleton;
    "test_lens_putr_iterate_multiple" >:: test_lens_putr_iterate_multiple;
    "test_lens_putr_identity" >:: test_lens_putr_identity;
    "test_inverse_putr" >:: test_inverse_putr;
  ]

let _ = run_test_tt_main lens_putr_suite


let test_lens_putl_const _ =
  assert_string_equal
    "source"
    (lens_putl RegexContext.empty (ConstLens ("source","target")) "target")

let test_lens_putl_concat _ =
  assert_string_equal
    "s1s2"
    (lens_putl
       RegexContext.empty
       (ConcatLens ((ConstLens ("s1","t1")),ConstLens ("s2","t2")))
       "t1t2")

let test_lens_putl_swap _ =
  assert_string_equal
    "s1s2"
    (lens_putl
       RegexContext.empty
       (SwapLens ((ConstLens ("s1","t1")),ConstLens ("s2","t2")))
       "t2t1")

let test_lens_putl_union_left _ =
  assert_string_equal
    "s1"
    (lens_putl
       RegexContext.empty
       (UnionLens ((ConstLens ("s1","t1")),ConstLens ("s2","t2")))
       "t1")

let test_lens_putl_union_right _ =
  assert_string_equal
    "s2"
    (lens_putl
       RegexContext.empty
       (UnionLens ((ConstLens ("s1","t1")),ConstLens ("s2","t2")))
       "t2")

let test_lens_putl_compose _ =
  assert_string_equal
    "s"
    (lens_putl
       RegexContext.empty
       (ComposeLens ((ConstLens ("t","u")),ConstLens ("s","t")))
       "u")

let test_lens_putl_iterate_empty _ =
  assert_string_equal
    ""
    (lens_putl
       RegexContext.empty
       (IterateLens (ConstLens ("s","t")))
       "")

let test_lens_putl_iterate_singleton _ =
  assert_string_equal
    "s"
    (lens_putl
       RegexContext.empty
       (IterateLens (ConstLens ("s","t")))
       "t")

let test_lens_putl_iterate_multiple _ =
  assert_string_equal
    "sss"
    (lens_putl
       RegexContext.empty
       (IterateLens (ConstLens ("s","t")))
       "ttt")

let test_lens_putl_identity _ =
  assert_string_equal
    "target"
    (lens_putl
       RegexContext.empty
       (IdentityLens (RegExBase "target"))
       "target")

let test_lens_putl_inverse _ =
  assert_string_equal
    "target"
    (lens_putl
       RegexContext.empty
       (InverseLens (ConstLens ("source","target")))
       "source")

let lens_putl_suite = "lens_putl Unit Tests" >:::
  [
    "test_lens_putl_const"             >:: test_lens_putl_const;
    "test_lens_putl_concat"            >:: test_lens_putl_concat;
    "test_lens_putl_swap"              >:: test_lens_putl_swap;
    "test_lens_putl_union_left"        >:: test_lens_putl_union_left;
    "test_lens_putl_union_right"       >:: test_lens_putl_union_right;
    "test_lens_putl_compose"           >:: test_lens_putl_compose;
    "test_lens_putl_iterate_empty"     >:: test_lens_putl_iterate_empty;
    "test_lens_putl_iterate_singleton" >:: test_lens_putl_iterate_singleton;
    "test_lens_putl_iterate_multiple"  >:: test_lens_putl_iterate_multiple;
    "test_lens_putl_identity"          >:: test_lens_putl_identity;
    "test_lens_putl_inverse"           >:: test_lens_putl_inverse;
  ]

let _ = run_test_tt_main lens_putl_suite

let test_dnf_lens_option (expected:dnf_lens option) (actual:dnf_lens option) =
  assert_equal
    ~printer:(fun l -> begin match l with
    | None -> "None"
    | Some l -> "Some " ^ Pp.pp_dnf_lens l end)
    expected
    actual

let test_generated_output_option
    (expected:(dnf_lens) option)
    (actual:(dnf_lens*regex*regex*RegexContext.t) option) =
  let actual = Option.map ~f:(fun (l,_,_,_) -> l) actual in
  test_dnf_lens_option expected actual

let test_gen_dnf_lens_const_nosoln _ =
  test_generated_output_option
    None
    (gen_dnf_lens RegexContext.empty
      (RegExBase "x")
      (RegExBase "y")
      [("a","b")]
      false)

let test_gen_dnf_lens_const_soln _ =
  test_generated_output_option
    (Some ([[],Permutation.create [], ["x"], ["y"]],Permutation.create [0]))
    (gen_dnf_lens RegexContext.empty
      (RegExBase "x")
      (RegExBase "y")
      [("x","y")]
      false)

let test_gen_lenses_union _ =
  test_generated_output_option
    (Some
      ([[],Permutation.create [], ["a"], ["y"];
        [],Permutation.create [], ["b"], ["x"]],
      Permutation.create [1;0]))
    (gen_dnf_lens RegexContext.empty
      (RegExOr (RegExBase "a", RegExBase "b"))
      (RegExOr (RegExBase "x", RegExBase "y"))
      [("a","y");("b","x")]
      false)

let test_gen_lenses_three_union _ =
  test_generated_output_option
    (Some
      ([[],Permutation.create [], ["a"], ["y"];
        [],Permutation.create [], ["b"], ["z"];
        [],Permutation.create [], ["c"], ["x"]],
      Permutation.create [2;0;1]))
    (gen_dnf_lens RegexContext.empty
      (RegExOr (RegExBase "a", RegExOr (RegExBase "b", RegExBase "c")))
      (RegExOr (RegExBase "x", RegExOr (RegExBase "y", RegExBase "z")))
      [("a","y");("b","z");("c","x")]
      false)

let test_gen_lenses_userdef_ident _ =
  test_generated_output_option
    (Some
      ([[AIdentity "A"], Permutation.create [0], ["";""], ["";""]],
      Permutation.create [0]))
    (gen_dnf_lens (RegexContext.create_from_list_exn ["A",RegExBase "a",false; "B", RegExBase "b",false])
      (RegExUserDefined "A")
      (RegExUserDefined "A")
      []
      false)

let test_gen_lenses_concat_userdef _ =
  test_generated_output_option
    (Some
      ([[AIdentity "A"; AIdentity "B"], Permutation.create [1;0],
        ["";"";""], ["";"";""]],
      Permutation.create [0]))
    (gen_dnf_lens (RegexContext.create_from_list_exn ["A",RegExBase "a",true; "B", RegExBase "b",true])
      (RegExConcat (RegExUserDefined "A", RegExUserDefined "B"))
      (RegExConcat (RegExUserDefined "B", RegExUserDefined "A"))
      ["ab","ba"]
      false)

let test_gen_lenses_concat_userdef_hard _ =
  test_generated_output_option
    (Some
      ([[AIdentity "A"; AIdentity "A"], Permutation.create [1;0],
        ["";"";""], ["";"";""]],
      Permutation.create [0]))
    (gen_dnf_lens (RegexContext.create_from_list_exn ["A",RegExOr (RegExBase "a", RegExBase "A"),true])
      (RegExConcat (RegExUserDefined "A", RegExUserDefined "A"))
      (RegExConcat (RegExUserDefined "A", RegExUserDefined "A"))
      [("Aa","aA")]
      false)

let test_gen_lenses_userdef_expand _ =
  test_generated_output_option
    (Some
      ([[], Permutation.create[], ["a"], ["a"]],
        Permutation.create [0]))
    (gen_dnf_lens (RegexContext.create_from_list_exn ["A", RegExBase "a",false])
      (RegExUserDefined "A")
      (RegExBase "a") []
      false)

let test_gen_lenses_star _ =
  test_generated_output_option
  (Some ([[(AIterate ([[], Permutation.create [], ["a"], ["b"]], Permutation.create
  [0]))], Permutation.create [0], ["";""], ["";""]], Permutation.create [0]))
  (gen_dnf_lens RegexContext.empty
      (RegExStar (RegExBase "a"))
      (RegExStar (RegExBase "b"))
      ["aa","bb"]
      false)

let test_gen_dnf_lens_star_difficult _ =
  test_generated_output_option
  (Some ([
    [(AIterate ([[], Permutation.create [], ["a"], ["b"]], Permutation.create
    [0]));
    (AIterate ([[], Permutation.create [], ["b"], ["a"]], Permutation.create
    [0]))
    ], Permutation.create [1;0], ["";"";""], ["";"";""]
    ],
    Permutation.create [0]))
  (gen_dnf_lens RegexContext.empty
      (RegExConcat
        (RegExStar (RegExBase "a"),
        RegExStar (RegExBase "b")))
      (RegExConcat
        (RegExStar (RegExBase "a"),
        RegExStar (RegExBase "b")))
      ["abb","aab"]
    false)

let test_dnf_lens_star_expansion _ =
  test_generated_output_option
  (Some ([
    [],Permutation.create [], [""], [""];
    [(AIterate ([[], Permutation.create [], ["a"], ["a"]], Permutation.create
    [0]))], Permutation.create [0], ["a";""], ["a";""]
    ],
    Permutation.create [0;1]))
  (gen_dnf_lens RegexContext.empty
      (RegExStar (RegExBase "a"))
      (RegExOr
        (RegExBase "",
        RegExConcat (RegExBase "a", RegExStar (RegExBase "a"))))
      ["a","a"]
      true)


let test_dnf_lens_star_inner_expansion _ =
  test_generated_output_option
    ( Some ([
      [AIterate ([
        ([],Permutation.create [], ["a"], ["a"]);
        ([AIterate ([
          ([],Permutation.create [], ["z"], ["z"])
        ],Permutation.create [0])],Permutation.create [0], ["az";""], ["az";""])
      ],Permutation.create [0;1])],Permutation.create [0], ["";""],
      ["";""]
      ],Permutation.create [0]))
    (gen_dnf_lens RegexContext.empty
      (RegExStar (RegExConcat (RegExBase "a", RegExStar (RegExBase
      "z"))))
      (RegExStar (RegExOr (RegExBase "a", RegExConcat (RegExBase
      "az", RegExStar (RegExBase "z")))))
      []
      false)

let test_dnf_lens_quotient_expansion _ =
  test_generated_output_option
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
    (gen_dnf_lens RegexContext.empty
      (RegExStar (RegExBase "a"))
      (RegExOr (RegExStar (RegExBase "aa"),
                            RegExConcat (RegExBase "a", RegExStar (RegExBase
                            "aa"))))
      []
      false)

let test_dnf_lens_inner_quotient_expansion _ =
  test_generated_output_option
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
      ], Permutation.create [0;1])
    ], Permutation.create [0], ["";""], ["";""])], Permutation.create [0]))
  (gen_dnf_lens RegexContext.empty
      (RegExStar (RegExConcat (RegExBase "a", RegExStar (RegExBase
    "b"))))
      (RegExStar (RegExConcat (RegExBase "a", RegExOr (RegExStar (RegExBase "bb"),
                            RegExConcat (RegExBase "b", RegExStar (RegExBase
                            "bb"))))))
      [("a","a")]
    false)

let gen_dnf_lens_suite = "gen_dnf_lens Unit Tests" >:::
  ["test_gen_dnf_lens_const_nosoln" >:: test_gen_dnf_lens_const_nosoln;
   "test_gen_dnf_lens_const_soln" >:: test_gen_dnf_lens_const_soln;
   "test_gen_lenses_union" >:: test_gen_lenses_union;
   "test_gen_lenses_three_union" >:: test_gen_lenses_three_union;
   "test_gen_lenses_userdef_ident" >:: test_gen_lenses_userdef_ident;
   "test_gen_lenses_concat_userdef" >:: test_gen_lenses_concat_userdef;
   "test_gen_lenses_userdef_expand" >:: test_gen_lenses_userdef_expand;
   "test_gen_lenses_concat_userdef_hard" >:: test_gen_lenses_concat_userdef_hard;
   "test_gen_lenses_star" >:: test_gen_lenses_star;
   "test_gen_dnf_lens_star_difficult" >:: test_gen_dnf_lens_star_difficult;
   "test_dnf_lens_star_expansion" >:: test_dnf_lens_star_expansion;
   "test_dnf_lens_star_inner_expansion" >:: test_dnf_lens_star_inner_expansion;
   "test_dnf_lens_quotient_expansion" >:: test_dnf_lens_quotient_expansion;
   "test_dnf_lens_inner_quotient_expansion" >:: test_dnf_lens_inner_quotient_expansion;
  ]

let _ = run_test_tt_main gen_dnf_lens_suite



let test_to_swap_concat_compose_tree_singleton _ =
  assert_swap_concat_compose_tree_equal
    (Permutation.to_swap_concat_compose_tree
      (Permutation.create [0]))
    (SCCTLeaf)

let test_to_swap_concat_compose_tree_ordered_double _ =
  assert_swap_concat_compose_tree_equal
    (Permutation.to_swap_concat_compose_tree
      (Permutation.create [0;1]))
    (SCCTConcat
      (SCCTLeaf,
      SCCTLeaf))

let test_to_swap_concat_compose_tree_swapped_double _ =
  assert_swap_concat_compose_tree_equal
    (Permutation.to_swap_concat_compose_tree
      (Permutation.create [1;0]))
    (SCCTSwap
      (SCCTLeaf,
      SCCTLeaf))

let test_to_swap_concat_compose_tree_swapped_triple _ =
  assert_swap_concat_compose_tree_equal
    (Permutation.to_swap_concat_compose_tree
      (Permutation.create [0;2;1]))
    (SCCTConcat
      ((SCCTLeaf),
      SCCTSwap
        (SCCTLeaf,
        SCCTLeaf)))

let test_to_swap_concat_compose_tree_cyclic_triple _ =
  assert_swap_concat_compose_tree_equal
    (Permutation.to_swap_concat_compose_tree
      (Permutation.create [2;1;0]))
    (SCCTSwap
      (SCCTSwap
        (SCCTLeaf,
        SCCTLeaf),
      SCCTLeaf))

let test_to_swap_concat_compose_tree_cyclic_satanic _ =
  assert_swap_concat_compose_tree_equal
    (Permutation.to_swap_concat_compose_tree
    (Permutation.create [2;0;3;1]))
    (SCCTCompose
      (SCCTConcat
        (SCCTSwap
          (SCCTConcat
            (SCCTLeaf, SCCTLeaf)
          ,SCCTLeaf)
        ,SCCTLeaf)
      ,SCCTConcat
        (SCCTConcat
          (SCCTLeaf,SCCTLeaf)
        ,SCCTSwap
          (SCCTLeaf,SCCTLeaf))))

let to_swap_concat_compose_tree_suite = "to_swap_concat_compose_tree Unit Tests" >:::
  [
    "test_to_swap_concat_compose_tree_singleton" >:: test_to_swap_concat_compose_tree_singleton;
    "test_to_swap_concat_compose_tree_ordered_double" >:: test_to_swap_concat_compose_tree_ordered_double;
    "test_to_swap_concat_compose_tree_swapped_double" >:: test_to_swap_concat_compose_tree_swapped_double;
    "test_to_swap_concat_compose_tree_swapped_triple" >:: test_to_swap_concat_compose_tree_swapped_triple;
    "test_to_swap_concat_compose_tree_cyclic_triple" >:: test_to_swap_concat_compose_tree_cyclic_triple;
    "test_to_swap_concat_compose_tree_cyclic_satanic" >:: test_to_swap_concat_compose_tree_cyclic_satanic;
  ]

let _ = run_test_tt_main to_swap_concat_compose_tree_suite




let test_atom_lens_to_lens_basic _ =
  assert_lens_equal
    (IdentityLens (RegExBase "TODO"))
    (IdentityLens (RegExBase "TODO"))

let atom_lens_to_lens_suite = "atom_lens_to_lens Unit Tests" >:::
  [
    "test_atom_lens_to_lens_basic" >:: test_atom_lens_to_lens_basic;
  ]

let _ = run_test_tt_main atom_lens_to_lens_suite


