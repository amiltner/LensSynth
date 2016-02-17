open OUnit
open Lang

(* Lang tests *)
let test_to_normalized_exp_base _ =
  assert_equal
    (to_normalized_exp (RegExBase 'x'))
    [[NRXBase 'x']]


let to_normalized_exp_suite = "to_normalized_exp Unit Tests" >:::
  ["test_to_normalized_exp_base" >:: test_to_normalized_exp_base]

let _ = run_test_tt_main to_normalized_exp_suite
