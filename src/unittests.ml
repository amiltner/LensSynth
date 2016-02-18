open OUnit
open Lang
open Pp

(* Lang tests *)
let test_normalizization (expected:normalized_regex) (actual:normalized_regex) =
  assert_equal
    ~printer:pp_normalized_regex
    expected
    actual

let test_to_normalized_exp_base _ =
  test_normalizization
    (to_normalized_exp (RegExBase 'x'))
    [[NRXBase 'x']]

let test_to_normalized_exp_concat_easy _ =
  test_normalizization
    [[NRXBase 'a'; NRXBase 'b']]
    (to_normalized_exp
      (RegExConcat
        (RegExBase 'a',
        RegExBase 'b')))
    
let test_to_normalized_exp_concat_tree _ =
  test_normalizization
    [[NRXBase 'a'; NRXBase 'b'; NRXBase 'c'; NRXBase 'd'; NRXBase 'e']]
    (to_normalized_exp
      (RegExConcat
        (RegExConcat
          (RegExBase 'a',
          RegExBase 'b'),
        RegExConcat
          (RegExBase 'c',
          RegExConcat
            (RegExBase 'd',
            RegExBase 'e')))))

let test_to_normalized_exp_union_tree _ =
  test_normalizization
    [[NRXBase 'a']; [NRXBase 'b']; [NRXBase 'c']; [NRXBase 'd']; [NRXBase 'e']]
    (to_normalized_exp
      (RegExOr
        (RegExOr
          (RegExBase 'a',
          RegExBase 'b'),
        RegExOr
          (RegExBase 'c',
          RegExOr
            (RegExBase 'd',
            RegExBase 'e')))))

let test_to_normalized_exp_append_distributeconcat _ =
  test_normalizization
    (to_normalized_exp
      (RegExConcat
        (RegExConcat
          (RegExBase 'a',
          RegExOr
            (RegExBase 'b',
            RegExBase 'c')),
        RegExBase 'd')))
    [[NRXBase 'a'; NRXBase 'b'; NRXBase 'd'];
     [NRXBase 'a'; NRXBase 'c'; NRXBase 'd']]

let test_to_normalized_exp_complicated _ =
  test_normalizization
    (to_normalized_exp
    (RegExConcat
      (RegExConcat
        (RegExBase 'a',
          RegExOr
            (RegExBase 'b',
            RegExStar
              (RegExConcat
                (RegExConcat
                  (RegExBase 'c',
                  RegExOr
                    (RegExBase 'd',
                    RegExBase 'e')),
                RegExBase 'f')))),
         RegExBase 'g')))
    [[NRXBase 'a'; NRXBase 'b'; NRXBase 'g'];
     [NRXBase 'a';
      NRXStar [
        [NRXBase 'c'; NRXBase 'd'; NRXBase 'f'];
        [NRXBase 'c'; NRXBase 'e'; NRXBase 'f']
      ];
      NRXBase 'g']]


let to_normalized_exp_suite = "to_normalized_exp Unit Tests" >:::
  ["test_to_normalized_exp_base" >:: test_to_normalized_exp_base;
   "test_to_normalized_exp_concat_easy" >:: test_to_normalized_exp_concat_easy;
   "test_to_normalized_exp_concat_tree" >:: test_to_normalized_exp_concat_tree;
   "test_to_normalized_exp_union_tree" >:: test_to_normalized_exp_union_tree;
   "test_to_normalized_exp_append_distributeconcat" >:: test_to_normalized_exp_append_distributeconcat;
   "test_to_normalized_exp_complicated" >:: test_to_normalized_exp_complicated]

let _ = run_test_tt_main to_normalized_exp_suite
