open Core.Std
open Lang
open Regexcontext

let likelihood_of_continuing_star = 0.9

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

let _ = Random.self_init()
