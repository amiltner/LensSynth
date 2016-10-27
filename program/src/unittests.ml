open Core.Std
open Normalized_lang
open Converter
open Counters
open Priority_queue
open Disjointset
open Regexcontext
open Language_equivalences
open Lenscontext
open Lens_utilities
open Permutation
open Ounit_extensions
open Ounit_general_extensions
open Util
open OUnit2
open Eval
open Lang
open Gen
open Lens_put
open Boom_lang
open Quotient_regex

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
let ds_1231equal : int DisjointSet.t =
  DisjointSet.create_from_equivalences (=)
    [(1,2);(2,3);(3,1)]

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

let test_disjointset_cyclic _ =
  assert_int_equal
    (DisjointSet.find_representative ds_1231equal 1)
    (DisjointSet.find_representative ds_1231equal 3)
  

let disjointset_suite = "DisjointSet Unit Tests" >:::
  [
    "test_disjointset_singleton" >:: test_disjointset_singleton;
    "test_disjointset_setequiv"  >:: test_disjointset_setequiv ;
    "test_disjointset_disjoint"  >:: test_disjointset_disjoint ;
    "test_disjointset_cyclic"    >:: test_disjointset_cyclic   ;
  ]

let _ = run_test_tt_main disjointset_suite


let test_to_exampled_dnf_constant_noex _ =
  assert_exampled_dnf_option_equal
    (Some ([([],["a"],[])],[]))
    (regex_to_exampled_dnf_regex RegexContext.empty LensContext.empty
      (RegExBase "a")
      [])

let test_to_exampled_dnf_constant_2ex _ =
  assert_exampled_dnf_option_equal
    (Some ([([],["a"],[[1];[0]])],[[1];[0]]))
    (regex_to_exampled_dnf_regex RegexContext.empty LensContext.empty
      (RegExBase "a")
      ["a";"a"])

let test_to_exampled_dnf_or _ =
  assert_exampled_dnf_option_equal
    (Some ([([],["a"],[[1]]);([],["b"],[[0]])],[[1];[0]]))
    (regex_to_exampled_dnf_regex RegexContext.empty LensContext.empty
      (RegExOr (RegExBase "a", RegExBase "b"))
      ["b";"a"])

let test_to_exampled_dnf_userdefined _ =
  assert_exampled_dnf_option_equal
    (Some ([([EAVariable ("A","A",LensIdentity(RegExVariable "A"),["a"],[[0]])],["";""],[[0]])],[[0]]))
    (regex_to_exampled_dnf_regex (RegexContext.create_from_list_exn ["A",RegExBase "a",true]) LensContext.empty
      (RegExVariable "A")
      ["a"])

let test_to_exampled_dnf_star _ =
  assert_exampled_dnf_option_equal
  (Some ([([EAStar (([[],["a"],[[1;0];[0;0]]],[[1;0];[0;0]]),[[0]])],["";""],[[0]])],[[0]]))
    (regex_to_exampled_dnf_regex RegexContext.empty LensContext.empty
      (RegExStar (RegExBase "a"))
      ["aa"])

(*let test_to_exampled_dnf_star_udef_or_concat _ =
  assert_exampled_dnf_option_equal
    (Some ([],[[0]]))
    (regex_to_exampled_dnf_regex ["A",RegExBase "a"]
      (RegExConcat
        ((RegExStar (RegExVariable "A"))
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
    (compare_exampled_dnf_regexs ([[EAVariable
    ("a","a",LensIdentity (RegExVariable "a"),["a";"aa"],[[0];[1]])],["";"1qaz"],[[0];[1]]],[[0];[1]])
    ([[EAVariable
    ("a","a",LensIdentity (RegExVariable "a"),["a";"aa"],[[0];[1]])],["";"2wsx"],[[0];[1]]],[[0];[1]]))

let test_compare_exampled_dnf_regexs_userdefineds_lt1 _ =
  assert_comparison_equal
    LT
    (compare_exampled_dnf_regexs ([[EAVariable
    ("a","a",LensIdentity (RegExVariable "a"),["b"],[[0]])],["";"1qaz"],[[0]]],[[0]])
    ([[EAVariable ("b","b",LensIdentity (RegExVariable "b"),["a"],[[0]])],["";"2wsx"],[[0]]],[[0]]))

let test_compare_exampled_dnf_regexs_userdefineds_lt2 _ =
  assert_comparison_equal
    LT
    (compare_exampled_dnf_regexs ([[EAVariable
    ("a","a",LensIdentity (RegExVariable "a"), ["a"],[[0]])],["";"1qaz"],[[0]]],[[0]])
    ([[EAVariable ("a","a", LensIdentity (RegExVariable "a"), ["b"],[[0]])],["";"2wsx"],[[0]]],[[0]]))

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
    (Failure (test_rxc_concrete_name ^ " already exists in the context"))
    (fun _ ->
       RegexContext.insert_exn
         test_rxc_context
         test_rxc_concrete_name
         test_rxc_abstract_base
         false)

let test_lookup_for_expansion_empty _ =
  assert_raises
    (Failure "bad regex name: none")
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

let test_lc_a_name = "a"
let test_lc_b_name = "b"
let test_lc_c_name = "c"
let test_lc_a = RegExVariable test_lc_a_name
let test_lc_b = RegExVariable test_lc_b_name
let test_lc_c = RegExVariable test_lc_c_name
let test_lc_ab_name = "ab"
let test_lc_bc_name = "bc"
let test_lc_ca_name = "ca"
let test_lc_ab_lens = LensConst ("a","b")
let test_lc_bc_lens = LensConst ("b","c")
let test_lc_ca_lens = LensConst ("c","a")
let test_lc_ab_variable_lens = LensVariable test_lc_ab_name
let test_lc_bc_variable_lens = LensVariable test_lc_bc_name
let test_lc_ca_variable_lens = LensVariable test_lc_ca_name
let test_lc_context =
  LensContext.create_from_list_exn
    [(test_lc_ab_name,test_lc_ab_lens,test_lc_a,test_lc_b)
    ;(test_lc_bc_name,test_lc_bc_lens,test_lc_b,test_lc_c)
    ;(test_lc_ca_name,test_lc_ca_lens,test_lc_c,test_lc_a)]

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
    (test_lc_ab_lens, test_lc_a, test_lc_b)
    (LensContext.lookup_exn test_lc_context test_lc_ab_name)

let test_lc_lookup_type _ =
  assert_regex_regex_equal
    (test_lc_a, test_lc_b)
    (LensContext.lookup_type_exn test_lc_context test_lc_ab_name)

let test_lc_lookup_impl _ =
  assert_lens_equal
    (test_lc_ab_lens)
    (LensContext.lookup_impl_exn test_lc_context test_lc_ab_name)

let test_lc_insert_conflicted _ =
  assert_raises
    (Failure "bad insert")
    (fun _ ->
       LensContext.insert_exn
         test_lc_context
         test_lc_ab_name
         test_lc_ab_lens
         test_lc_a
         test_lc_b)

let test_lc_shortest_path_exn_nopath _ =
  assert_raises
    (Failure "regexes not in same equivalence class")
    (fun _ ->
       LensContext.shortest_path_exn
         test_lc_context
         test_lc_a_name
         "other")

let test_lc_shortest_path_exn_normal _ =
  assert_lens_equal
    test_lc_bc_variable_lens
    (LensContext.shortest_path_exn
       test_lc_context
       test_lc_b_name
       test_lc_c_name)

let test_lc_shortest_path_exn_inverse _ =
  assert_lens_equal
    (LensInverse test_lc_ca_variable_lens)
    (LensContext.shortest_path_exn
       test_lc_context
       test_lc_a_name
       test_lc_c_name)

let test_lc_paths_to_rep_elt_1 _ =
  assert_id_lens_equal
    (test_lc_c_name,LensIdentity (test_lc_c))
    (LensContext.shortest_path_to_rep_elt
       test_lc_context
       test_lc_c_name)

let test_lc_paths_to_rep_elt_2 _ =
  assert_id_lens_equal
    (test_lc_c_name,test_lc_bc_variable_lens)
    (LensContext.shortest_path_to_rep_elt
       test_lc_context
       test_lc_b_name)

let test_lc_paths_to_rep_elt_3 _ =
  assert_id_lens_equal
    (test_lc_c_name,LensInverse test_lc_ca_variable_lens)
    (LensContext.shortest_path_to_rep_elt
       test_lc_context
       test_lc_a_name)

let test_lc_paths_to_rep_elt_singleton _ =
  assert_id_lens_equal
    ("sinlgetonname",LensIdentity (RegExVariable "singletonname"))
    (LensContext.shortest_path_to_rep_elt
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
    "test_lc_shortest_path_exn_nopath" >:: test_lc_shortest_path_exn_nopath;
    "test_lc_shortest_path_exn_normal" >:: test_lc_shortest_path_exn_normal;
    "test_lc_shortest_path_exn_inverse" >:: test_lc_shortest_path_exn_inverse;
    "test_lc_paths_to_rep_elt_1" >:: test_lc_paths_to_rep_elt_1;
    "test_lc_paths_to_rep_elt_2" >:: test_lc_paths_to_rep_elt_2;
    "test_lc_paths_to_rep_elt_3" >:: test_lc_paths_to_rep_elt_3;
  ]

let _ = run_test_tt_main lens_context_suite





(* Eval tests *)
let test_fast_eval_base_positive _ =
  assert_bool_equal
    true
    (fast_eval RegexContext.empty (RegExBase "x") "x")

let test_fast_eval_base_negative1 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty (RegExBase "x") "y")

let test_fast_eval_base_negative2 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty (RegExBase "x") "xx")

let test_fast_eval_concat_positive1 _ =
  assert_bool_equal
    true
    (fast_eval RegexContext.empty
      (RegExConcat
        (RegExBase "x",
        RegExConcat
          (RegExBase "y",
          RegExBase "z"))) "xyz")

let test_fast_eval_concat_positive2 _ =
  assert_bool_equal
    true
    (fast_eval RegexContext.empty
      (RegExConcat
        (RegExConcat
          (RegExBase "x",
          RegExBase "y"),
        RegExBase "z")) "xyz")

let test_fast_eval_concat_negative1 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty
      (RegExConcat
        (RegExBase "x",
        RegExBase "y")) "x")

let test_fast_eval_concat_negative2 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty
      (RegExConcat
        (RegExBase "x",
        RegExBase "y")) "xz")

let test_fast_eval_concat_negative3 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty
      (RegExConcat
        (RegExBase "x",
        RegExBase "y")) "yx")

let test_fast_eval_concat_negative4 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty
      (RegExConcat
        (RegExBase "x",
        RegExBase "y")) "xyz")

let test_fast_eval_or_positive _ =
  assert_bool_equal
    true
    (fast_eval RegexContext.empty
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
    (fast_eval RegexContext.empty
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
    (fast_eval RegexContext.empty
      (RegExStar
        (RegExBase "a")) "")

let test_fast_eval_star_one _ =
  assert_bool_equal
    true
    (fast_eval RegexContext.empty
      (RegExStar
        (RegExBase "a")) "a")

let test_fast_eval_star_two _ =
  assert_bool_equal
    true
    (fast_eval RegexContext.empty
      (RegExStar
        (RegExBase "a")) "aa")

let test_fast_eval_star_choice _ =
  assert_bool_equal
    true
    (fast_eval RegexContext.empty
      (RegExStar
        (RegExOr
          (RegExBase "a"
          ,RegExBase "b"))
      ) "aab" )

let test_fast_eval_star_negative1 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty
      (RegExStar
        (RegExBase "a")) "b")

let test_fast_eval_star_negative2 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty
      (RegExStar
        (RegExBase "a")) "ab")

let test_fast_eval_star_negative3 _ =
  assert_bool_equal
    false
    (fast_eval RegexContext.empty
      (RegExStar
        (RegExBase "a")) "ba")

let test_fast_eval_userdef_positive _ =
  assert_bool_equal
    true
    (fast_eval (RegexContext.create_from_list_exn ["A",RegExBase "a",true])
      (RegExVariable "A") "a")

let test_fast_eval_userdef_negative _ =
  assert_bool_equal
    false
    (fast_eval (RegexContext.create_from_list_exn ["A",RegExBase "a",true])
      (RegExVariable "A") "b")

let test_fast_eval_concat_userdef _ =
  assert_bool_equal
  true
  (fast_eval
    (RegexContext.create_from_list_exn [("A", RegExBase "a",true);("B", RegExBase "b",true)])
    (RegExConcat (RegExVariable "A", RegExVariable "B"))
    "ab")

let test_fast_eval_nested_userdef _ =
  assert_bool_equal
  true
  (fast_eval
   (RegexContext.create_from_list_exn [("A", RegExBase "a", true);("B", RegExVariable "A",true)])
   (RegExVariable "B") "a")

let test_fast_eval_fast _ =
  assert_bool_equal
  true
  (fast_eval
     (RegexContext.create_from_list_exn [("A", RegExConcat (RegExBase "c", RegExConcat (RegExStar
  (RegExBase "a"), RegExStar (RegExBase "b"))),true)])
  (RegExConcat (RegExStar (RegExVariable "A"), RegExConcat (RegExBase "z",
  RegExStar (RegExConcat (RegExConcat (RegExVariable "A", RegExBase "q"),
  RegExStar (RegExConcat (RegExVariable "A", RegExOr (RegExBase "t",
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
       (ERegExVariable ("t",["userdefined";"not"],[[0];[1]]))
       [0])

let extract_string_suite = "extract_string Unit Tests" >:::
  [
    "test_extract_string_base" >:: test_extract_string_base;
    "test_extract_string_concat" >:: test_extract_string_concat;
    "test_extract_string_union_left" >:: test_extract_string_union_left;
    "test_extract_string_union_right" >:: test_extract_string_union_right;
    "test_extract_string_iterate" >:: test_extract_string_iterate;
    "test_extract_string_userdefined" >:: test_extract_string_userdefined;
  ]

let _ = run_test_tt_main extract_string_suite

(*todo:putr using lens library funct *)
let test_lens_putr_const _ =
  assert_string_equal
    "target"
    (lens_putr RegexContext.empty LensContext.empty (LensConst ("source","target")) "source")

let test_lens_putr_concat _ =
  assert_string_equal
    "t1t2"
    (lens_putr
       RegexContext.empty
       LensContext.empty
       (LensConcat ((LensConst ("s1","t1")),LensConst ("s2","t2")))
       "s1s2")

let test_lens_putr_swap _ =
  assert_string_equal
    "t2t1"
    (lens_putr
       RegexContext.empty
       LensContext.empty
       (LensSwap ((LensConst ("s1","t1")),LensConst ("s2","t2")))
       "s1s2")

let test_lens_putr_union_left _ =
  assert_string_equal
    "t1"
    (lens_putr
       RegexContext.empty
       LensContext.empty
       (LensUnion ((LensConst ("s1","t1")),LensConst ("s2","t2")))
       "s1")

let test_lens_putr_union_right _ =
  assert_string_equal
    "t2"
    (lens_putr
       RegexContext.empty
       LensContext.empty
       (LensUnion ((LensConst ("s1","t1")),LensConst ("s2","t2")))
       "s2")

let test_lens_putr_compose _ =
  assert_string_equal
    "u"
    (lens_putr
       RegexContext.empty
       LensContext.empty
       (LensCompose ((LensConst ("t","u")),LensConst ("s","t")))
       "s")

let test_lens_putr_iterate_empty _ =
  assert_string_equal
    ""
    (lens_putr
       RegexContext.empty
       LensContext.empty
       (LensIterate (LensConst ("s","t")))
       "")

let test_lens_putr_iterate_singleton _ =
  assert_string_equal
    "t"
    (lens_putr
       RegexContext.empty
       LensContext.empty
       (LensIterate (LensConst ("s","t")))
       "s")

let test_lens_putr_iterate_multiple _ =
  assert_string_equal
    "ttt"
    (lens_putr
       RegexContext.empty
       LensContext.empty
       (LensIterate (LensConst ("s","t")))
       "sss")

let test_lens_putr_identity _ =
  assert_string_equal
    "source"
    (lens_putr
       RegexContext.empty
       LensContext.empty
       (LensIdentity (RegExBase "source"))
       "source")

let test_inverse_putr _ =
  assert_string_equal
    "source"
    (lens_putr
       RegexContext.empty
       LensContext.empty
       (LensInverse (LensConst ("source","target")))
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
    (lens_putl RegexContext.empty LensContext.empty (LensConst ("source","target")) "target")

let test_lens_putl_concat _ =
  assert_string_equal
    "s1s2"
    (lens_putl
       RegexContext.empty
       LensContext.empty
       (LensConcat ((LensConst ("s1","t1")),LensConst ("s2","t2")))
       "t1t2")

let test_lens_putl_swap _ =
  assert_string_equal
    "s1s2"
    (lens_putl
       RegexContext.empty
       LensContext.empty
       (LensSwap ((LensConst ("s1","t1")),LensConst ("s2","t2")))
       "t2t1")

let test_lens_putl_union_left _ =
  assert_string_equal
    "s1"
    (lens_putl
       RegexContext.empty
       LensContext.empty
       (LensUnion ((LensConst ("s1","t1")),LensConst ("s2","t2")))
       "t1")

let test_lens_putl_union_right _ =
  assert_string_equal
    "s2"
    (lens_putl
       RegexContext.empty
       LensContext.empty
       (LensUnion ((LensConst ("s1","t1")),LensConst ("s2","t2")))
       "t2")

let test_lens_putl_compose _ =
  assert_string_equal
    "s"
    (lens_putl
       RegexContext.empty
       LensContext.empty
       (LensCompose ((LensConst ("t","u")),LensConst ("s","t")))
       "u")

let test_lens_putl_iterate_empty _ =
  assert_string_equal
    ""
    (lens_putl
       RegexContext.empty
       LensContext.empty
       (LensIterate (LensConst ("s","t")))
       "")

let test_lens_putl_iterate_singleton _ =
  assert_string_equal
    "s"
    (lens_putl
       RegexContext.empty
       LensContext.empty
       (LensIterate (LensConst ("s","t")))
       "t")

let test_lens_putl_iterate_multiple _ =
  assert_string_equal
    "sss"
    (lens_putl
       RegexContext.empty
       LensContext.empty
       (LensIterate (LensConst ("s","t")))
       "ttt")

let test_lens_putl_identity _ =
  assert_string_equal
    "target"
    (lens_putl
       RegexContext.empty
       LensContext.empty
       (LensIdentity (RegExBase "target"))
       "target")

let test_lens_putl_inverse _ =
  assert_string_equal
    "target"
    (lens_putl
       RegexContext.empty
       LensContext.empty
       (LensInverse (LensConst ("source","target")))
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

let test_gen_dnf_lens_const_nosoln _ =
  assert_dnf_lens_option_equal
    None
    (gen_dnf_lens RegexContext.empty
       LensContext.empty
      (RegExBase "x")
      (RegExBase "y")
      [("a","b")])

let test_gen_dnf_lens_const_soln _ =
  assert_dnf_lens_option_equal
    (Some ([[],Permutation.create [], ["x"], ["y"]],Permutation.create [0]))
    (gen_dnf_lens RegexContext.empty LensContext.empty
      (RegExBase "x")
      (RegExBase "y")
      [("x","y")])

let test_gen_lenses_union _ =
  assert_dnf_lens_option_equal
    (Some
      ([[],Permutation.create [], ["a"], ["y"];
        [],Permutation.create [], ["b"], ["x"]],
      Permutation.create [1;0]))
    (gen_dnf_lens RegexContext.empty LensContext.empty
      (RegExOr (RegExBase "a", RegExBase "b"))
      (RegExOr (RegExBase "x", RegExBase "y"))
      [("a","y");("b","x")])

let test_gen_lenses_three_union _ =
  assert_dnf_lens_option_equal
    (Some
      ([[],Permutation.create [], ["a"], ["y"];
        [],Permutation.create [], ["b"], ["z"];
        [],Permutation.create [], ["c"], ["x"]],
      Permutation.create [2;0;1]))
    (gen_dnf_lens RegexContext.empty LensContext.empty
      (RegExOr (RegExBase "a", RegExOr (RegExBase "b", RegExBase "c")))
      (RegExOr (RegExBase "x", RegExOr (RegExBase "y", RegExBase "z")))
      [("a","y");("b","z");("c","x")])

let test_gen_lenses_userdef_ident _ =
  assert_dnf_lens_option_equal
    (Some
      ([[AtomLensVariable (LensIdentity (RegExVariable "A"))], Permutation.create [0], ["";""], ["";""]],
      Permutation.create [0]))
    (gen_dnf_lens (RegexContext.create_from_list_exn ["A",RegExBase "a",false; "B", RegExBase "b",false]) LensContext.empty
      (RegExVariable "A")
      (RegExVariable "A")
      [])

let test_gen_lenses_concat_userdef _ =
  assert_dnf_lens_option_equal
    (Some
      ([[AtomLensVariable (LensIdentity (RegExVariable "A")); AtomLensVariable (LensIdentity (RegExVariable "B"))], Permutation.create [1;0],
        ["";"";""], ["";"";""]],
      Permutation.create [0]))
    (gen_dnf_lens (RegexContext.create_from_list_exn ["A",RegExBase "a",true; "B", RegExBase "b",true]) LensContext.empty
      (RegExConcat (RegExVariable "A", RegExVariable "B"))
      (RegExConcat (RegExVariable "B", RegExVariable "A"))
      ["ab","ba"])

let test_gen_lenses_concat_userdef_hard _ =
  assert_dnf_lens_option_equal
    (Some
      ([[AtomLensVariable(LensIdentity(RegExVariable "A")); AtomLensVariable(LensIdentity(RegExVariable "A"))], Permutation.create [1;0],
        ["";"";""], ["";"";""]],
      Permutation.create [0]))
    (gen_dnf_lens (RegexContext.create_from_list_exn ["A",RegExOr (RegExBase "a", RegExBase "A"),true]) LensContext.empty
      (RegExConcat (RegExVariable "A", RegExVariable "A"))
      (RegExConcat (RegExVariable "A", RegExVariable "A"))
      [("Aa","aA")])

let test_gen_lenses_userdef_expand _ =
  assert_dnf_lens_option_equal
    (Some
      ([[], Permutation.create[], ["a"], ["a"]],
        Permutation.create [0]))
    (gen_dnf_lens (RegexContext.create_from_list_exn ["A", RegExBase "a",false]) LensContext.empty
      (RegExVariable "A")
      (RegExBase "a") [])

let test_gen_lenses_star _ =
  assert_dnf_lens_option_equal
  (Some ([[(AtomLensIterate ([[], Permutation.create [], ["a"], ["b"]], Permutation.create
  [0]))], Permutation.create [0], ["";""], ["";""]], Permutation.create [0]))
  (gen_dnf_lens RegexContext.empty LensContext.empty
      (RegExStar (RegExBase "a"))
      (RegExStar (RegExBase "b"))
      ["aa","bb"])

let test_gen_dnf_lens_star_difficult _ =
  assert_dnf_lens_option_equal
  (Some ([
    [(AtomLensIterate ([[], Permutation.create [], ["a"], ["b"]], Permutation.create
    [0]));
    (AtomLensIterate ([[], Permutation.create [], ["b"], ["a"]], Permutation.create
    [0]))
    ], Permutation.create [1;0], ["";"";""], ["";"";""]
    ],
    Permutation.create [0]))
  (gen_dnf_lens RegexContext.empty LensContext.empty
      (RegExConcat
        (RegExStar (RegExBase "a"),
        RegExStar (RegExBase "b")))
      (RegExConcat
        (RegExStar (RegExBase "a"),
        RegExStar (RegExBase "b")))
      ["abb","aab"])

let test_dnf_lens_star_expansion _ =
  assert_dnf_lens_option_equal
  (Some ([
    [],Permutation.create [], [""], [""];
    [(AtomLensIterate ([[], Permutation.create [], ["a"], ["a"]], Permutation.create
    [0]))], Permutation.create [0], ["a";""], ["a";""]
    ],
    Permutation.create [0;1]))
  (gen_dnf_lens RegexContext.empty LensContext.empty
      (RegExStar (RegExBase "a"))
      (RegExOr
        (RegExBase "",
        RegExConcat (RegExBase "a", RegExStar (RegExBase "a"))))
      ["a","a"])


let test_dnf_lens_star_inner_expansion _ =
  assert_dnf_lens_option_equal
    ( Some ([
      [AtomLensIterate ([
        ([],Permutation.create [], ["a"], ["a"]);
        ([AtomLensIterate ([
          ([],Permutation.create [], ["z"], ["z"])
        ],Permutation.create [0])],Permutation.create [0], ["az";""], ["az";""])
      ],Permutation.create [0;1])],Permutation.create [0], ["";""],
      ["";""]
      ],Permutation.create [0]))
    (gen_dnf_lens RegexContext.empty LensContext.empty
      (RegExStar (RegExConcat (RegExBase "a", RegExStar (RegExBase
      "z"))))
      (RegExStar (RegExOr (RegExBase "a", RegExConcat (RegExBase
      "az", RegExStar (RegExBase "z")))))
      [])

let test_dnf_lens_quotient_expansion _ =
  assert_dnf_lens_option_equal
    (Some (
      [
        ([
          (AtomLensIterate ([[],Permutation.create [], ["aa"],["aa"]], Permutation.create [0]))
        ],
        Permutation.create [0],
        ["";""],
        ["";""]);
        ([
          (AtomLensIterate ([[],Permutation.create [], ["aa"],["aa"]], Permutation.create [0]))
        ],
        Permutation.create [0],
        ["a";""],
        ["a";""]);
      ], Permutation.create [0;1]))
    (gen_dnf_lens RegexContext.empty LensContext.empty
      (RegExStar (RegExBase "a"))
      (RegExOr (RegExStar (RegExBase "aa"),
                            RegExConcat (RegExBase "a", RegExStar (RegExBase
                            "aa"))))
      [])

let test_dnf_lens_inner_quotient_expansion _ =
  assert_dnf_lens_option_equal
  (Some (
    [
      ([
        AtomLensIterate 
(
      [
        ([
          (AtomLensIterate ([[],Permutation.create [], ["bb"],["bb"]], Permutation.create [0]))
        ],
        Permutation.create [0],
        ["a";""],
        ["a";""]);
        ([
          (AtomLensIterate ([[],Permutation.create [], ["bb"],["bb"]], Permutation.create [0]))
        ],
        Permutation.create [0],
        ["ab";""],
        ["ab";""]);
      ], Permutation.create [0;1])
    ], Permutation.create [0], ["";""], ["";""])], Permutation.create [0]))
  (gen_dnf_lens RegexContext.empty LensContext.empty
      (RegExStar (RegExConcat (RegExBase "a", RegExStar (RegExBase
    "b"))))
      (RegExStar (RegExConcat (RegExBase "a", RegExOr (RegExStar (RegExBase "bb"),
                            RegExConcat (RegExBase "b", RegExStar (RegExBase
                            "bb"))))))
      [("a","a")])

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
    (LensIdentity (RegExBase "TODO"))
    (LensIdentity (RegExBase "TODO"))

let atom_lens_to_lens_suite = "atom_lens_to_lens Unit Tests" >:::
  [
    "test_atom_lens_to_lens_basic" >:: test_atom_lens_to_lens_basic;
  ]

let _ = run_test_tt_main atom_lens_to_lens_suite



let test_simplify_lens_lensvariable _ =
  assert_lens_equal
    (LensVariable "var")
    (simplify_lens (LensVariable "var"))

let test_simplify_lens_combine_identity_or _ =
  assert_lens_equal
    (LensIdentity (RegExOr(RegExVariable "A", RegExVariable "B")))
    (simplify_lens
       (LensUnion
          (LensIdentity (RegExVariable "A")
          ,LensIdentity (RegExVariable "B"))))


let test_simplify_lens_combine_identity_concat _ =
  assert_lens_equal
    (LensIdentity (RegExConcat(RegExVariable "A", RegExVariable "B")))
    (simplify_lens
       (LensConcat
          (LensIdentity (RegExVariable "A")
          ,LensIdentity (RegExVariable "B"))))

let test_simplify_lens_distribute_iteration _ =
  assert_lens_equal
    (LensIdentity (RegExStar (RegExVariable "A")))
    (simplify_lens
       (LensIterate
          (LensIdentity (RegExVariable "A"))))

let test_simplify_lens_distribute_inverses _ =
  assert_lens_equal
    (LensConcat
       (LensConcat
          (LensIdentity (RegExVariable "A")
          ,LensConcat
              (LensConst ("B","b")
              ,LensIdentity (RegExVariable "C")))
       ,LensConst ("D","d")))
    (simplify_lens
       (LensInverse
          (LensConcat
             (LensConcat
                (LensIdentity (RegExVariable "A")
                ,LensConst ("b","B"))
             ,LensConcat
                 (LensIdentity (RegExVariable "C")
                 ,LensConst ("d","D"))))))

let test_simplify_lens_identify_consts _ =
  assert_lens_equal
    (LensIdentity (RegExBase "a"))
    (simplify_lens (LensConst ("a","a")))

let test_simplify_lens_combine_regex_consts _ =
  assert_lens_equal
    (LensIdentity (RegExBase "ab"))
    (simplify_lens (LensConcat (LensConst ("a","a"), LensConst ("b","b"))))


let simplify_lens_suite = "simplify_lens Unit Tests" >:::
  [
    "test_simplify_lens_lensvariable" >:: test_simplify_lens_lensvariable;
    "test_simplify_lens_combine_identity_or" >:: test_simplify_lens_combine_identity_or;
    "test_simplify_lens_combine_identity_concat" >:: test_simplify_lens_combine_identity_concat;
    "test_simplify_lens_distribute_iteration" >:: test_simplify_lens_distribute_iteration;
    "test_simplify_lens_distribute_inverses" >:: test_simplify_lens_distribute_inverses;
    "test_simplify_lens_identify_consts" >:: test_simplify_lens_identify_consts;
    "test_simplify_lens_combine_regex_consts" >:: test_simplify_lens_combine_regex_consts;
  ]

let _ = run_test_tt_main simplify_lens_suite

let test_quotient_whole_empty _ =
  assert_regex_equal
    (whole QuotientRegExEmpty)
    (RegExEmpty)

let test_quotient_whole_permute _ =
  assert_regex_equal
    (whole (QuotientRegExPermute ([QuotientRegExBase "hello"; QuotientRegExBase "world"], RegExBase ",")))
    (RegExOr (RegExConcat (RegExBase "world", RegExConcat (RegExBase ",", RegExBase "hello")),
              RegExConcat (RegExBase "hello", RegExConcat (RegExBase ",", RegExBase "world"))))

let test_quotient_whole_map _ =
  assert_regex_equal
    (whole (QuotientRegExMap (RegExOr (RegExBase "hello", RegExBase "Hello"), "hello")))
    (RegExOr (RegExBase "hello", RegExBase "Hello"))

let test_quotient_kernel_map _ =
  assert_regex_equal
    (kernel (QuotientRegExMap (RegExOr (RegExBase "hello", RegExBase "Hello"), "hello")))
    (RegExBase "hello")

let test_quotient_kernel_permute _ =
  assert_regex_equal
    (kernel (QuotientRegExPermute ([QuotientRegExBase "hello"; QuotientRegExBase "world"], RegExBase ",")))
    (RegExConcat (RegExBase "hello", RegExConcat (RegExBase ",", RegExBase "world")))

let quotient_regex_suite = "Quotient Regex Unit Tests" >:::
  [
    "test_quotient_whole_empty" >:: test_quotient_whole_empty;
    "test_quotient_whole_permute" >:: test_quotient_whole_permute;
    "test_quotient_whole_map" >:: test_quotient_whole_map;
    "test_quotient_kernel_map" >:: test_quotient_kernel_map;
    "test_quotient_kernel_permute" >:: test_quotient_kernel_permute;
  ]

let _ = run_test_tt_main quotient_regex_suite

let test_boom_program_of_program_userdef_abstract _ =
  assert_boom_program_equal
    [BoomStmtDefinition("r",BoomTypRegex,BoomExpRegex (RegExBase "b"))]
    (boom_program_of_program LensContext.empty [DeclRegexCreation("r",RegExBase "b",true)])

let test_boom_program_of_program_userdef_concrete _ =
  assert_boom_program_equal
    [BoomStmtDefinition("r",BoomTypRegex,BoomExpRegex (RegExBase "b"))]
    (boom_program_of_program LensContext.empty [DeclRegexCreation("r",RegExBase "b",false)])

let test_boom_program_of_program_test_string _ =
  assert_boom_program_equal
    [BoomStmtTestRegex (RegExBase "b", "c")]
    (boom_program_of_program LensContext.empty [DeclTestString (RegExBase "b", "c")])

let test_boom_program_of_program_synthesize_program _ =
  assert_raises
    (Failure "no boom functionality for this")
    (fun _ ->
       boom_program_of_program LensContext.empty
         [DeclSynthesizeLens ("n",RegExBase "a", RegExBase "b", [])])

let test_boom_program_of_program_lens_creation _ =
  assert_boom_program_equal
    [BoomStmtDefinition
       ("n"
       ,BoomTypLens(RegExBase "a",RegExBase "b")
       ,BoomExpLens(LensVariable "x"))]
    (boom_program_of_program LensContext.empty
       [DeclLensCreation ("n", RegExBase "a", RegExBase "b", LensVariable "x")])

let boom_program_of_program_suite = "boom_program_of_program Unit Tests" >:::
  [
    "test_boom_program_of_program_userdef_abstract" >:: test_boom_program_of_program_userdef_abstract;
    "test_boom_program_of_program_userdef_concrete" >:: test_boom_program_of_program_userdef_concrete;
    "test_boom_program_of_program_test_string" >:: test_boom_program_of_program_test_string;
    "test_boom_program_of_program_synthesize_program" >:: test_boom_program_of_program_synthesize_program;
    "test_boom_program_of_program_lens_creation" >:: test_boom_program_of_program_lens_creation;
  ]

let _ = run_test_tt_main boom_program_of_program_suite
