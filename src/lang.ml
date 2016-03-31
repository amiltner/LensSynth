open Core.Std
open Printf
open Util

(**** Language {{{ *****)

exception Internal_error of string
let internal_error f s = raise @@ Internal_error (sprintf "(%s) %s" f s)

type regex =
  | RegExBase of string
  | RegExConcat of regex * regex
  | RegExOr of regex * regex
  | RegExStar of regex
  | RegExUserDefined of string

type examples = (string * string) list

type synth_problem = (string * regex) list * regex * regex
                     * (string * string) list

type unioned_subex = concated_subex list

and concated_subex = basis_subex list

and basis_subex =
  | NRXBase of string
  | NRXStar of unioned_subex
  | NRXUserDefined of string

type normalized_regex = unioned_subex

type normalized_synth_problem = ((string * normalized_regex) list)
                                * normalized_regex * normalized_regex
                                * (string * string) list

type context = (string * regex) list

let rec to_normalized_exp (r:regex) : normalized_regex =
  begin match r with
  | RegExBase c -> [[NRXBase c]]
  | RegExConcat (r1,r2) ->
      cartesian_map (@) (to_normalized_exp r1) (to_normalized_exp r2)
  | RegExOr (r1,r2) -> (to_normalized_exp r1) @ (to_normalized_exp r2)
  | RegExStar (r') -> [[NRXStar (to_normalized_exp r')]]
  | RegExUserDefined s -> [[NRXUserDefined s]]
  end

let rec to_normalized_synth_problem ((c,r1,r2,es):synth_problem)
: normalized_synth_problem =
  (List.map ~f:(fun (s,r) -> (s, to_normalized_exp r)) c, to_normalized_exp r1, to_normalized_exp r2, es)



type atom =
  | AUserDefined of string
  | AStar of dnf_regex

and clause = atom list * string list

and dnf_regex = clause list

let empty_string : dnf_regex = [([],[""])]

let rec concat_dnf_regexs (r1:dnf_regex) (r2:dnf_regex) : dnf_regex =
  cartesian_map
    (fun (a1s,s1s) (a2s,s2s) -> (a1s@a2s,weld_lists (^) s1s s2s))
    r1
    r2

let rec or_dnf_regexs (r1:dnf_regex) (r2:dnf_regex) : dnf_regex =
  r1 @ r2

let rec concat_clause_dnf_rx (cl:clause) (r:dnf_regex) : dnf_regex =
  concat_dnf_regexs [cl] r

let rec concat_dnf_rx_clause (r:dnf_regex) (cl:clause) : dnf_regex =
  concat_dnf_regexs r [cl]

let rec exponentiate (r:dnf_regex) (n:int) : dnf_regex =
  if n < 0 then
    failwith "invalid exponential"
  else if n = 0 then
    empty_string
  else
    concat_dnf_regexs (exponentiate r (n-1)) r

let rec quotiented_star (r:dnf_regex) (n:int) : dnf_regex =
  if n < 1 then
    failwith "invalid modulation"
  else if n = 1 then
    empty_string
  else
    or_dnf_regexs (quotiented_star r (n-1)) (exponentiate r (n-1))

let rec singleton_atom (a:atom) : dnf_regex =
  [([a],["";""])]

let rec to_dnf_regex (r:regex) : dnf_regex =
  let rec atom_to_dnf_regex (a:atom) : dnf_regex =
    [([a],["";""])]
  in
  begin match r with
  | RegExBase c -> [([],[c])]
  | RegExConcat (r1,r2) ->
      cartesian_map
        (fun (a1s,s1s) (a2s,s2s) -> (a1s@a2s,weld_lists (^) s1s s2s))
        (to_dnf_regex r1)
        (to_dnf_regex r2)
  | RegExOr (r1, r2) -> (to_dnf_regex r1) @ (to_dnf_regex r2)
  | RegExStar (r') -> atom_to_dnf_regex (AStar (to_dnf_regex r'))
  | RegExUserDefined s -> atom_to_dnf_regex (AUserDefined s)
  end


(***** }}} *****)
