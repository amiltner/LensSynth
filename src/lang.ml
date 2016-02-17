open Core.Std
open Printf

(**** Language {{{ *****)

type regex =
  | RegExBase of char
  | RegExConcat of regex * regex
  | RegExOr of regex * regex
  | RegExStar of regex

type unionable_subex = concatable_subex list

and basis_subex =
  | NRXBase of char
  | NRXStar of unionable_subex

and concatable_subex = basis_subex list

type normalized_regex = unionable_subex

let to_normalized_exp (r:regex) : normalized_regex =
  failwith "unimplemented"

(***** }}} *****)
