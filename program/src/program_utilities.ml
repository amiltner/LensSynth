open Core.Std
open Lang
open Qre_context
open Quotient_regex
open Regexcontext
open Eval
open Gen
open Lens_put
open Quotientlenscontext

let run_declaration
    (qc:QuotientRegexContext.t)
    (rc:RegexContext.t)
    (qlc:QuotientLensContext.t)
    (d:declaration)
  : QuotientRegexContext.t * RegexContext.t * QuotientLensContext.t * declaration =
  begin match d with
    | DeclQuotientRegexCreation (n,q,b) ->
      (QuotientRegexContext.insert_exn qc n q (not b), rc, qlc, d)
    | DeclQuotientLensCreation (n,r1,r2,l) ->
      (qc,rc,QuotientLensContext.insert_exn qlc n l r1 r2, d)
    | DeclQuotientSynthesizeLens (n,r1,r2,exs) ->
      let lo = gen_quotient_lens qc (QuotientLensContext.get_lens_context qlc) r1 r2 exs in
      begin match lo with
        | None -> failwith (n ^ " has no satisfying lens")
        | Some l ->
          (qc,rc,QuotientLensContext.insert_exn qlc n l r1 r2,
          DeclQuotientLensCreation(n,r1,r2,l))
      end
    | DeclQuotientTestString (r, s) ->
        if fast_eval (QuotientRegexContext.to_whole_regex_context qc) (whole r) s then
          (qc, rc, qlc, d)
        else
          failwith (s ^ " does not match regex " ^ (regex_to_string (whole r)))
    | DeclQuotientTestLens (n, exs) ->  
        List.iter
        ~f:(fun (lex,rex) ->
            let (ans, rex) = quotient_lens_putr qc qlc n (lex, rex) in
            if ans <> rex then
              failwith ("expected:" ^ rex ^ "got:" ^ ans)
            else
              ())
        exs;
      (qc,rc,qlc,d)
  end

let synthesize_and_load_program
    (p:program)
  : QuotientRegexContext.t * RegexContext.t * QuotientLensContext.t * program =
  let (qc,rc,lc,p) = List.fold_left
      ~f:(fun (qc,rc,lc,p) d ->
          let (qc,rc,lc,d) = run_declaration qc rc lc d in
          (qc,rc,lc,d::p))
      ~init:(QuotientRegexContext.empty, RegexContext.empty, QuotientLensContext.empty, [])
      p
  in
  (qc,rc,lc,List.rev p)

let synthesize_program
    (p:program)
  : program =
  let (_,_,_,p) = synthesize_and_load_program p in
  p

let load_program
    (p:program)
  : QuotientRegexContext.t * RegexContext.t * QuotientLensContext.t =
  let (qc,rc,lc,_) = synthesize_and_load_program p in
  (qc,rc,lc)

let remove_tests : program -> program =
  List.filter
    ~f:(fun d ->
        begin match d with
          | DeclQuotientRegexCreation _ -> true
          | DeclQuotientSynthesizeLens _ -> true
          | DeclQuotientLensCreation _ -> false
          | DeclQuotientTestString _ -> false
          | DeclQuotientTestLens _ -> false
        end)

let retrieve_last_synthesis_problem_exn
    (p:program)
  : QuotientRegexContext.t * RegexContext.t * QuotientLensContext.t * regex * regex * examples =
  let p = remove_tests p in
  let (qc,rc,lc,last_synth_option) = List.fold_left
    ~f:(fun (qc,rc,lc,last_synth_option) d ->
        begin match (d,last_synth_option) with
          | (_,_) ->
            let (qc,rc,lc,_) = run_declaration qc rc lc d in
            (qc,rc,lc,last_synth_option)
        end)
    ~init:(QuotientRegexContext.empty,RegexContext.empty,QuotientLensContext.empty,None)
    p
  in
  let (_,r1,r2,exs) = Option.value_exn last_synth_option in
  (qc,rc,lc,r1,r2,exs)
