open OUnit2
open Core.Std
open String_utilities


let assert_not_equal (printer:'a -> string) (expectednot:'a) (actual:'a) =
  assert_bool
    ("Expected and Actual are not equal, value=" ^
       (printer expectednot))
    (expectednot <> actual)

let assert_bool_equal (expected:bool) (actual:bool) =
  assert_equal
    ~printer:string_of_bool
    expected
    actual

let assert_true (actual:bool) =
  assert_bool_equal
    true
    actual

let assert_false (actual:bool) =
  assert_bool_equal
    false
    actual

let assert_not_equal_bool (expected_not:bool) (actual:bool) =
  assert_not_equal string_of_bool expected_not actual

let assert_not_equal_int (expected_not:int) (actual:int) =
  assert_not_equal string_of_int expected_not actual

let assert_int_equal = assert_equal ~printer:string_of_int

let assert_float_equal = assert_equal ~printer:Float.to_string

let assert_int_option_equal =
  assert_equal
    ~printer:(fun int_option -> begin match int_option with
    | None -> "None"
    | Some x -> string_of_int x
      end)

let assert_float_option_equal =
  assert_equal
    ~printer:(string_of_option Float.to_string)

let assert_string_double_option_equal
  (expected:(string*string) option) (actual:(string*string) option) =
  assert_equal
    ~printer:(fun x -> begin match x with
              | None -> "None"
              | Some (x1,x2) -> "(" ^ x1 ^ "," ^ x2 ^ ")" end)
    expected
    actual

let assert_string_list_option_equal
  (expected:(string list) option) (actual:(string list) option) =
  assert_equal
    ~printer:(fun x -> begin match x with
              | None -> "None"
              | Some xs -> "[" ^ (String.concat xs ~sep:";") ^ "]" end)
    expected
    actual

let assert_ordered_string_assoc_list_equal =
  assert_equal
    ~printer:(fun counts ->     "[" ^ (String.concat ~sep:" ; " (List.map ~f:(fun (x,c) -> (x) ^ "->" ^
    (Float.to_string c)) counts)) ^ "]")


let assert_comparison_equal =
  assert_equal
    ~printer:string_of_comparison

let assert_char_list_list_equal = assert_equal
    ~printer:string_of_char_list_list

let assert_char_list_double_equal = assert_equal

let assert_string_equal = assert_equal
  ~printer:ident
