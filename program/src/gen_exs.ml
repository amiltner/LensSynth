open Core.Std
open Lang
open Regexcontext
open Normalized_lang
open Language_equivalences

let likelihood_of_continuing_star = 0.95

let rec gen_element_of_regex_language
    (c:RegexContext.t)
    (r:regex)
  : string =
  begin match r with
  | RegExBase s -> s
  | RegExConcat (r1,r2) -> (gen_element_of_regex_language c r1) ^
                           (gen_element_of_regex_language c r2)
  | RegExOr (r1,r2) ->
      if Random.bool() then
        gen_element_of_regex_language c r1
      else
        gen_element_of_regex_language c r2
  | RegExStar r' ->
      if Random.float 1.0 < likelihood_of_continuing_star then
        gen_element_of_regex_language c r' ^ gen_element_of_regex_language c r
      else
        ""
  | RegExVariable t ->
    let rex = RegexContext.lookup_exn c t in
    gen_element_of_regex_language c rex
  | RegExEmpty ->
    failwith "no elements of this language"
  end

let rec gen_element_of_dnf_regex
    (c:RegexContext.t)
    (d:dnf_regex)
  : string =
  let clause_num = Random.int (List.length d) in
  gen_element_of_clause c (List.nth_exn d clause_num)

and gen_element_of_clause
    (c:RegexContext.t)
    ((atoms,strings):clause)
  : string =
  let atom_strings = List.map ~f:(gen_element_of_atom c) atoms in
  begin match strings with
    | [] -> failwith "bad"
    | h::t ->
      let atomstringdouble = List.zip_exn atom_strings t in
      List.fold_left
        ~f:(fun acc (atom_string,string_string) ->
            acc ^ atom_string ^ string_string)
        ~init:h
        atomstringdouble
  end
    
and gen_element_of_atom
    (c:RegexContext.t)
    (a:atom)
  : string =
  begin match a with
    | AUserDefined s ->
      let r = RegexContext.lookup_exn c s in
      let d = to_dnf_regex r in
      gen_element_of_dnf_regex c d
    | AStar d ->
      if Random.float 1.0 < likelihood_of_continuing_star then
        gen_element_of_atom c a ^ gen_element_of_dnf_regex c d
      else
        ""
  end

