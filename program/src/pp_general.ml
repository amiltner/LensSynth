open Core.Std
open Util

let paren (s:string) : string = "(" ^ s ^ ")"

let bracket (s:string) : string = "[" ^ s ^ "]"

let pp_option (inner_pp:'a -> string) (ao:'a option) : string =
  begin match ao with
    | None -> "None"
    | Some a -> inner_pp a
  end

let pp_list (inner_pp:'a -> string) (al:'a list) : string =
  bracket
    (String.concat
       ~sep:";"
       (List.map ~f:inner_pp al))

let pp_double (first_pp:'a -> string) (second_pp:'b -> string) ((a,b):('a*'b))
  : string =
  paren
    ((first_pp a) ^ "," ^ (second_pp b))

let pp_int_list : int list -> string =
  pp_list string_of_int

let pp_int_list_list : int list list -> string =
  pp_list pp_int_list

let pp_char_list : char list -> string =
  pp_list Char.to_string

let pp_char_list_list : char list list -> string =
  pp_list pp_char_list

let pp_comparison (c:comparison) : string =
  begin match c with
  | EQ -> "EQ"
  | LT -> "LT"
  | GT -> "GT"
  end
