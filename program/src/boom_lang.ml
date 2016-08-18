open Core.Std
open Util
open Lang
open Lenscontext
open Lens_utilities

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

let retrieve_inverses_of_lens_variables_exn (p:program) : id list =
  let rec retrieve_inverses_of_lens_variables_in_lens_exn
      (l:lens)
    : id list =
    begin match l with
      | LensConst _ -> []
      | LensConcat(l1,l2) ->
        let vs1 = retrieve_inverses_of_lens_variables_in_lens_exn l1 in
        let vs2 = retrieve_inverses_of_lens_variables_in_lens_exn l2 in
        vs1@vs2
      | LensSwap(l1,l2) ->
        let vs1 = retrieve_inverses_of_lens_variables_in_lens_exn l1 in
        let vs2 = retrieve_inverses_of_lens_variables_in_lens_exn l2 in
        vs1@vs2
      | LensUnion(l1,l2) ->
        let vs1 = retrieve_inverses_of_lens_variables_in_lens_exn l1 in
        let vs2 = retrieve_inverses_of_lens_variables_in_lens_exn l2 in
        vs1@vs2
      | LensCompose(l1,l2) ->
        let vs1 = retrieve_inverses_of_lens_variables_in_lens_exn l1 in
        let vs2 = retrieve_inverses_of_lens_variables_in_lens_exn l2 in
        vs1@vs2
      | LensIterate(l') ->
        retrieve_inverses_of_lens_variables_in_lens_exn l'
      | LensIdentity _ -> []
      | LensInverse(LensVariable n) -> [n]
      | LensInverse(l') ->
        retrieve_inverses_of_lens_variables_in_lens_exn l'
      | LensVariable _ -> []
    end
  in
  List.dedup
    (List.fold_left
       ~f:(fun acc d ->
           begin match d with
             | DeclLensCreation (_,_,_,l) ->
               acc @
               (retrieve_inverses_of_lens_variables_in_lens_exn l)
             | _ -> acc
           end)
       ~init:[]
       p)

let add_inverted_lenses_after_original_lenses
    (lc:LensContext.t)
    (p:program)
    (inverted_vars:id list)
  : LensContext.t * program * (id * id) list =
  List.fold_right
    ~f:(fun d (lc,p,id_map) ->
        begin match d with
          | DeclLensCreation (n,r1,r2,l) ->
            if List.mem inverted_vars n then
              let l_inv = simplify_lens (LensInverse l) in
              let n_inv = LensContext.autogen_id_from_base lc (n ^ "_inv") in
              let d_inv = DeclLensCreation(n_inv,r2,r1,l_inv) in
              let lc = LensContext.insert_exn lc n_inv l_inv r2 r1 in
              (lc,d::d_inv::p,(n,n_inv)::id_map)
            else
              (lc,d::p,id_map)
          | _ -> (lc,d::p,id_map)
        end)
    ~init:(lc,[],[])
    p

let replace_inverted_lens_variables
    (inverted_map:(id * id) list)
    (p:program)
  : program =
  let replace_inverted_lens_variables_current_level_lens
      (l:lens)
    : lens =
    begin match l with
      | LensInverse(LensVariable n) ->
        LensVariable (List.Assoc.find_exn inverted_map n)
      | _ -> l
    end
  in
  let replace_inverted_lens_variables_lens =
    apply_at_every_level_lens
      replace_inverted_lens_variables_current_level_lens
  in
  List.fold_right
    ~f:(fun d acc ->
        let d =
          begin match d with
            | DeclLensCreation (n,r1,r2,l) ->
              let l = replace_inverted_lens_variables_lens l in
              DeclLensCreation (n,r1,r2,l)
            | _ -> d
          end
        in
        d::acc)
    ~init:[]
    p

let boom_program_of_program
    (lc:LensContext.t)
    (p:program)
  : boom_statement list =
  let p =
    fold_until_completion
      (fun (lc,p) ->
         let inversed_vars = retrieve_inverses_of_lens_variables_exn p in
         if inversed_vars = [] then
           Right p
         else
           let (lc,p,inverted_map) =
             add_inverted_lenses_after_original_lenses
               lc
               p
               inversed_vars
           in
           let p = replace_inverted_lens_variables inverted_map p in
           Left (lc,p))
      (lc,p)
  in
  List.concat_map ~f:statement_of_decl p
