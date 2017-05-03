open Core.Std
open Normalized_lang
open Permutation
open Lang
open String_utilities
open Util
open Ounit_general_extensions
open Boom_lang
open Gen_exs


let assert_dnf_equal (expected:dnf_regex) (actual:dnf_regex) =
  assert_equal
    ~printer:dnf_regex_to_string
    ~cmp:compare_dnf_regexs
    expected
    actual

let assert_swap_concat_compose_tree_equal =
  assert_equal
    ~printer:pp_swap_concat_compose_tree
    ~cmp:compare_swap_concat_compare_tree

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
    ~cmp:(option_compare
            (pair_compare
               Permutation.compare
               (compare_list
                  ~cmp:(pair_compare int_compare int_compare))))

let assert_lens_equal =
  assert_equal
    ~printer:lens_to_string
    ~cmp:lens_compare

let assert_dnf_lens_option_equal =
  assert_equal
    ~printer:(String_utilities.string_of_option dnf_lens_to_string)
    ~cmp:(option_compare compare_dnf_lens)

let assert_exampled_dnf_option_equal =
  assert_equal
    ~printer:(string_of_option exampled_dnf_regex_to_string)
    ~cmp:(option_compare compare_exampled_dnf_regexs)

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
    ~cmp:regex_compare

let assert_regex_option_equal =
  assert_equal
    ~printer:(string_of_option regex_to_string)

let assert_lens_regex_regex_equal =
  assert_equal
    ~printer:(string_of_triple lens_to_string regex_to_string regex_to_string)
    ~cmp:(triple_compare lens_compare regex_compare regex_compare)

let assert_regex_regex_equal =
  assert_equal
    ~printer:(string_of_pair regex_to_string regex_to_string)
    ~cmp:(pair_compare regex_compare regex_compare)

let assert_lens_list_equal =
  assert_equal
    ~printer:(string_of_list lens_to_string)
    ~cmp:(compare_list ~cmp:lens_compare)

let assert_id_lens_list_equal =
  assert_equal
    ~printer:(string_of_pair ident (string_of_list lens_to_string))
    ~cmp:(pair_compare string_compare (compare_list ~cmp:lens_compare))

let assert_id_lens_equal =
  assert_equal
    ~printer:(string_of_pair ident lens_to_string)
    ~cmp:(pair_compare string_compare lens_compare)

let assert_boom_statement_equal =
  assert_equal
    ~printer:Pp.pp_statement
    ~cmp:compare_boom_statement

let assert_boom_expression_equal =
  assert_equal
    ~printer:Pp.pp_expression
    ~cmp:compare_boom_expression

let assert_boom_program_equal =
  assert_equal
    ~printer:Pp.pp_program
    ~cmp:compare_boom_program

module ModTenPQueue = Priority_queue_two.Make(
  struct
    type element = int
    let compare = comparison_compare
    let priority = (fun x -> (x mod 10))
    let to_string = string_of_int
  end)

let assert_int_int_int_priority_queue_option_equal =
  let printer =
    string_of_option
      (string_of_triple
         string_of_int
         string_of_int
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
    ~cmp:comparer

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
    ~cmp:comparer

module RegexIntSet = Comparison_set.Make(
  struct
    type element = regex * int
    let compare = pair_compare regex_compare int_compare
    let to_string = string_of_pair regex_to_string string_of_int
  end)
  
let assert_expansions_equal
    (ri1s:(regex * int) list)
    (ri2s:(regex * int) list) =
  let printer = string_of_list (string_of_pair regex_to_string string_of_int) in
  assert_equal
    ~printer:printer
    ~cmp:(ordered_partition_order (pair_compare regex_compare int_compare))
    ri1s
    ri2s

let assert_double_expansions_equal
    (ri1s:(regex * regex * int) list)
    (ri2s:(regex * regex * int) list) =
  let printer = string_of_list
      (string_of_triple regex_to_string regex_to_string string_of_int)
  in
  assert_equal
    ~printer:printer
    ~cmp:(ordered_partition_order
            (triple_compare regex_compare regex_compare int_compare))
    ri1s
    ri2s


let assert_ordered_string_assoc_list_equal =
  assert_equal
    ~printer:(string_of_list (string_of_pair ident string_of_int))
    ~cmp:(compare_list ~cmp:(pair_compare string_compare int_compare))

let assert_string_int_int_pair_list_pair_equal =
  assert_equal
    ~printer:(string_of_pair
                ident
                (string_of_list
                   (string_of_pair string_of_int string_of_int)))
    ~cmp:(pair_compare
            string_compare
            (compare_list
               ~cmp:(pair_compare int_compare int_compare)))

let assert_flattened_or_userdef_regex_list_equal =
  assert_equal
    ~printer:(string_of_list flattened_or_userdef_regex_to_string)
    ~cmp:(compare_list ~cmp:compare_flattened_or_userdef_regex)

let assert_int_int_list_equal =
  assert_equal
    ~printer:(string_of_list (string_of_pair string_of_int string_of_int))
    ~cmp:(compare_list ~cmp:(pair_compare int_compare int_compare))
