open Core.Std
open Printf
open Util

(**** Language {{{ *****)

type regex =
  | RegExBase of char
  | RegExConcat of regex * regex
  | RegExOr of regex * regex
  | RegExStar of regex

type synth_problem = regex * regex
                     * (string * string) list

type unioned_subex = concated_subex list

and concated_subex = basis_subex list

and basis_subex =
  | NRXBase of char
  | NRXStar of unioned_subex

type normalized_regex = unioned_subex

type normalized_synth_problem = normalized_regex * normalized_regex
                                * (string * string) list

let rec to_normalized_exp (r:regex) : normalized_regex =
  begin match r with
  | RegExBase c -> [[NRXBase c]]
  | RegExConcat (r1,r2) ->
      cartesian_map (@) (to_normalized_exp r1) (to_normalized_exp r2)
  | RegExOr (r1,r2) -> (to_normalized_exp r1) @ (to_normalized_exp r2)
  | RegExStar (r') -> [[NRXStar (to_normalized_exp r')]]
  end

let rec to_normalized_synth_problem ((r1,r2,es):synth_problem)
: normalized_synth_problem =
  (to_normalized_exp r1, to_normalized_exp r2, es)

(***** }}} *****)
