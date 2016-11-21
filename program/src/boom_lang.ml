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
    | _ -> failwith "Unclear how to implement" (* Quotient cases *)
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
      | LensPermute (_,ls) ->
        List.concat_map
          ~f:retrieve_inverses_of_lens_variables_in_lens_exn
          ls
    end
  in
  List.dedup
    (List.fold_left
       ~f:(fun acc d ->
           begin match d with
             | _ -> acc
           end)
       ~init:[]
       p)

let add_inverted_lenses_after_original_lenses
    (lc:LensContext.t)
    (p:program)
    (_:id list)
  : LensContext.t * program * (id * id) list =
  List.fold_right
    ~f:(fun d (lc,p,id_map) ->
        begin match d with
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
        begin match List.Assoc.find inverted_map n with
          | None ->
            l
          | Some v -> LensVariable v
        end
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
