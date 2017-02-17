open OUnit2
open Core.Std
open Normalized_lang
open Permutation
open Lang
open String_utilities
open Util


let assert_dnf_equal (expected:dnf_regex) (actual:dnf_regex) =
  assert_equal
    ~printer:dnf_regex_to_string
    expected
    actual

let assert_swap_concat_compose_tree_equal =
  assert_equal
    ~printer:pp_swap_concat_compose_tree

let assert_permutation_option_equal =
  assert_equal
    ~printer:(fun x -> begin match x with
                       | None -> "None"
                       | Some p -> "Some " ^ (Permutation.pp p)
                       end)

let assert_permutation_guesses_option_equal =
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

let assert_lens_equal =
  assert_equal
    ~printer:lens_to_string

let assert_dnf_lens_option_equal =
  assert_equal
    ~printer:(String_utilities.string_of_option dnf_lens_to_string)

let assert_exampled_dnf_option_equal =
  assert_equal
    ~printer:(fun ro ->
      begin match ro with
      | None -> "None"
      | Some r -> exampled_dnf_regex_to_string r
      end)

let assert_dnf_lens_equal =
  assert_equal
  ~printer:dnf_lens_to_string

let assert_regex_list_equal =
  assert_equal
  ~printer:(fun rs -> "[" ^
    (String.concat
    ~sep:";"
    (List.map ~f:regex_to_string rs))
                      ^ "]")

let assert_regex_equal =
  assert_equal
    ~printer:regex_to_string

let assert_regex_option_equal =
  assert_equal
    ~printer:(string_of_option regex_to_string)

let assert_lens_regex_regex_equal =
  assert_equal
    ~printer:(fun (l,r1,r2) ->
        paren (
          (lens_to_string l) ^ "," ^
          (regex_to_string r1) ^ "," ^
          (regex_to_string r2)))

let assert_regex_regex_equal =
  assert_equal
    ~printer:(fun (r1,r2) ->
        paren (
          (regex_to_string r1) ^ "," ^
          (regex_to_string r2)))

let assert_lens_list_equal =
  assert_equal
    ~printer:(string_of_list lens_to_string)

let assert_id_lens_list_equal =
  assert_equal
    ~printer:(string_of_pair ident (string_of_list lens_to_string))

let assert_id_lens_equal =
  assert_equal
    ~printer:(string_of_pair ident lens_to_string)

let assert_boom_statement_equal =
  assert_equal
    ~printer:Pp.pp_statement

let assert_boom_expression_equal =
  assert_equal
    ~printer:Pp.pp_expression

let assert_boom_program_equal =
  assert_equal
    ~printer:Pp.pp_program

module ModTenPQueue = Priority_queue_two.Make(
  struct
    type element = int
    let compare = comparison_compare
    let priority = (fun x -> Float.of_int (x mod 10))
    let to_string = string_of_int
  end)

let assert_int_float_int_priority_queue_option_equal =
  let printer =
    string_of_option
      (string_of_triple
         string_of_int
         Float.to_string
         ModTenPQueue.to_string)
  in
  let comparer =
    option_compare
      (triple_compare
         comparison_compare
         comparison_compare
         ModTenPQueue.compare)
  in
  assert_equal
    ~printer:printer
    ~cmp:(comparer_to_equality_check comparer)

let assert_required_expansions_equal =
  let printer =
    string_of_list
      (string_of_triple
         regex_to_string
         regex_to_string
         string_of_int)
  in
  let comparer =
    ordered_partition_order
      (triple_compare
         regex_compare
         regex_compare
         comparison_compare)
  in
  assert_equal
    ~printer:printer
    ~cmp:(comparer_to_equality_check comparer)
  
