open Core.Std
open Util

let paren (s:string) : string = "(" ^ s ^ ")"

let bracket (s:string) : string = "[" ^ s ^ "]"

let undelimit_string : string -> string =
    (Str.global_replace (Str.regexp "\\\\\\\\") "\\\\")
  % (Str.global_replace (Str.regexp "\\\\n") "\n")
  % (Str.global_replace (Str.regexp "\\\\t") "\t")
  % (Str.global_replace (Str.regexp "\\\\\"") "\"")

      
let delimit_string : string -> string =
  Str.global_replace (Str.regexp "\"") "\\\\\""
  % (Str.global_replace (Str.regexp "\n") "\\\\n")
  % (Str.global_replace (Str.regexp "\t") "\\\\t")
  % (Str.global_replace (Str.regexp "\\\\") "\\\\\\\\")

let string_of_option (inner_converter:'a -> string) (ao:'a option) : string =
  begin match ao with
    | None -> "None"
    | Some a -> inner_converter a
  end

let string_of_list (inner_converter:'a -> string) (al:'a list) : string =
  bracket
    (String.concat
       ~sep:";"
       (List.map ~f:inner_converter al))

let string_of_double
    (first_converter:'a -> string)
    (second_converter:'b -> string)
    ((a,b):('a*'b))
  : string =
  paren
    ((first_converter a) ^ "," ^ (second_converter b))

let string_of_int_list : int list -> string =
  string_of_list string_of_int

let string_of_int_list_list : int list list -> string =
  string_of_list string_of_int_list

let string_of_char_list : char list -> string =
  string_of_list Char.to_string

let string_of_char_list_list : char list list -> string =
  string_of_list string_of_char_list

let string_of_comparison (c:comparison) : string =
  begin match c with
  | EQ -> "EQ"
  | LT -> "LT"
  | GT -> "GT"
  end
