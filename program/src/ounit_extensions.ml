open OUnit2
open Core.Std
open Priority_queue
open Normalized_lang
open Permutation
open Lens
open Regex
open Pp
open String_utilities


let assert_dnf_equal (expected:dnf_regex) (actual:dnf_regex) =
  assert_equal
    ~printer:pp_dnf_regex_as_regex
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
    ~printer:string_of_lens

let assert_dnf_lens_option_equal =
  assert_equal
    ~printer:(String_utilities.string_of_option pp_dnf_lens)

let assert_int_float_int_priority_queue_option_equal =
  assert_equal
    ~printer:(fun x ->
      begin match x with
      | None -> "None"
      | Some (d,p,q) -> "(" ^ (string_of_int d) ^ "," ^ (Float.to_string p) ^
      "," ^
      (Priority_Queue.pp string_of_int q) ^ ")"
      end)

let assert_exampled_dnf_option_equal =
  assert_equal
    ~printer:(fun ro ->
      begin match ro with
      | None -> "None"
      | Some r -> string_of_exampled_dnf_regex r
      end)

let assert_dnf_lens_equal =
  assert_equal
  ~printer:Pp.pp_dnf_regex_as_regex

let assert_regex_list_equal =
  assert_equal
  ~printer:(fun rs -> "[" ^
    (String.concat
    ~sep:";"
    (List.map ~f:string_of_regex rs))
                      ^ "]")

let assert_regex_equal =
  assert_equal
    ~printer:string_of_regex

let assert_regex_option_equal =
  assert_equal
    ~printer:(string_of_option string_of_regex)

let assert_lens_regex_regex_equal =
  assert_equal
    ~printer:(fun (l,r1,r2) ->
        paren (
          (string_of_lens l) ^ "," ^
          (string_of_regex r1) ^ "," ^
          (string_of_regex r2)))

let assert_regex_regex_equal =
  assert_equal
    ~printer:(fun (r1,r2) ->
        paren (
          (string_of_regex r1) ^ "," ^
          (string_of_regex r2)))

let assert_lens_list_equal =
  assert_equal
    ~printer:(string_of_list string_of_lens)

let assert_id_lens_list_equal =
  assert_equal
    ~printer:(string_of_double ident (string_of_list string_of_lens))

let assert_id_lens_equal =
  assert_equal
    ~printer:(string_of_double ident string_of_lens)
