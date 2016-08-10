open Core.Std
open Printf
open Regex

(**** Language {{{ *****)

exception Internal_error of string
let internal_error f s = raise @@ Internal_error (sprintf "(%s) %s" f s)

type id = string








type examples = (string * string) list

type specification = (string * regex * regex * (string * string) list)

type declaration =
  | DeclUserdefCreation of (string * regex * bool)
  | DeclTestString of (regex * string)
  | DeclSynthesizeProgram of specification

type program = declaration list

type synth_problems = (string * regex * bool) list * (specification list) 

type exampled_regex =
  | ERegExEmpty
  | ERegExBase of string * (int list list)
  | ERegExConcat of exampled_regex * exampled_regex * (int list list)
  | ERegExOr of exampled_regex  * exampled_regex * (int list list)
  | ERegExStar of exampled_regex * (int list list)
  | ERegExVariable of string * string list * (int list list)
  | ERegExMapped of int * string list * (int list list)

let extract_iterations_consumed (er:exampled_regex) : int list list =
  begin match er with
    | ERegExEmpty -> []
    | ERegExBase (_,ill) -> ill
    | ERegExConcat (_,_,ill) -> ill
    | ERegExOr (_,_,ill) -> ill
    | ERegExStar (_,ill) -> ill
    | ERegExVariable (_,_,ill) -> ill
    | ERegExMapped (_,_,ill) -> ill
  end

let took_regex (er:exampled_regex)
    (iteration:int list) : bool =
  let ill = extract_iterations_consumed er in
  List.mem ill iteration

let rec extract_string (er:exampled_regex) (iteration:int list)
  : string =
  begin match er with
    | ERegExEmpty -> failwith "no string"
    | ERegExBase (s,_) -> s
    | ERegExConcat (er1,er2,_) ->
      (extract_string er1 iteration) ^
      (extract_string er2 iteration)
    | ERegExOr (er1,er2,_) ->
      if took_regex er1 iteration then
        extract_string er1 iteration
      else
        extract_string er2 iteration
    | ERegExStar (er',_) ->
        let valid_iterations =
          List.rev
            (List.filter
              ~f:(fun it -> List.tl_exn it = iteration)
              (extract_iterations_consumed er')) in
        String.concat
          (List.map
            ~f:(extract_string er')
            valid_iterations)
    | ERegExVariable (_,sl,ill) ->
        let dat_opt = List.findi
          ~f:(fun _ il -> il = iteration)
          ill in
        begin match dat_opt with
        | None -> failwith "im horrible"
        | Some (i,_) ->
            List.nth_exn sl i
        end
    | ERegExMapped (_,sl,ill) ->
        let dat_opt = List.findi
          ~f:(fun _ il -> il = iteration)
          ill in
        begin match dat_opt with
        | None -> failwith "im horrible"
        | Some (i,_) ->
            List.nth_exn sl i
        end
  end

let extract_example_list (er:exampled_regex) : int list list =
  begin match er with
  | ERegExEmpty -> []
  | ERegExBase (_,ill) -> ill
  | ERegExConcat (_,_,ill) -> ill
  | ERegExOr (_,_,ill) -> ill
  | ERegExStar (_,ill) -> ill
  | ERegExVariable (_,_,ill) -> ill
  | ERegExMapped (_,_,ill) -> ill
  end


(***** }}} *****)
