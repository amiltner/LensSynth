open Core.Std
open Lang

type boom_typ =
  | BoomTypRegex
  | BoomTypLens of regex * regex

type boom_expression =
  | BoomExpRegex of regex
  | BoomExpLens of lens
  | BoomExpCut of boom_statement * boom_expression

and boom_statement =
  | BoomStmtDefinition of id * boom_typ * boom_expression
  | BoomStmtTestRegex of regex * string
  | BoomStmtTestLens of lens * string * string

type boom_program = boom_statement list

let statement_of_decl (d:declaration) : boom_statement list =
  begin match d with
    | DeclRegexCreation(n,r,_) ->
      [BoomStmtDefinition (n,BoomTypRegex,BoomExpRegex r)]
    | DeclTestString (r,s) -> [BoomStmtTestRegex (r,s)]
    | DeclSynthesizeLens _ -> failwith "no boom functionality for this"
    | DeclLensCreation (n,r1,r2,l) ->
      [BoomStmtDefinition (n,BoomTypLens(r1,r2),BoomExpLens l)]
    | DeclTestLens (n,exs) ->
      List.map
        ~f:(fun (lex,rex) -> BoomStmtTestLens (LensVariable n, lex, rex))
        exs
  end

let boom_program_of_program : program -> boom_statement list =
  List.concat_map ~f:statement_of_decl
