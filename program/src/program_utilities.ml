open Core.Std
open Lang
open Qre_context
open Quotient_regex
open Regexcontext
open Lenscontext
open Eval
open Gen
open Lens_put

let run_declaration
    (qc:QuotientRegexContext.t)
    (rc:RegexContext.t)
    (lc:LensContext.t)
    (d:declaration)
  : QuotientRegexContext.t * RegexContext.t * LensContext.t * declaration =
  begin match d with
    | DeclRegexCreation (n,r,b) ->
      (qc, RegexContext.insert_exn rc n r (not b),lc,d)
    | DeclTestString (r,s) ->
      if fast_eval rc r s then
        (qc, rc,lc,d)
      else
        failwith (s ^ " does not match regex " ^ (regex_to_string r))
    | DeclSynthesizeLens (n,r1,r2,exs) ->
      let lo = gen_lens rc lc r1 r2 exs in
      begin match lo with
        | None -> failwith (n ^ " has no satisfying lens")
        | Some l ->
          (qc,rc,LensContext.insert_exn lc n l r1 r2,DeclLensCreation(n,r1,r2,l))
      end
    | DeclLensCreation (n,r1,r2,l) ->
      (qc, rc,LensContext.insert_exn lc n l r1 r2,d)
    | DeclTestLens (n,exs) ->
      List.iter
        ~f:(fun (lex,rex) ->
            let ans = lens_putr rc lc (LensVariable n) lex in
            if ans <> rex then
              failwith ("expected:" ^ rex ^ "got:" ^ ans)
            else
              ())
        exs;
      (qc,rc,lc,d)
    | DeclQuotientRegexCreation (n,q,b) ->
      (QuotientRegexContext.insert_exn qc n q (not b), rc, lc, d)
    | DeclQuotientLensCreation (n,r1,r2,l) ->
      (qc,rc,LensContext.insert_exn lc n l (kernel r1) (kernel r2), d)
    | DeclQuotientSynthesizeLens (n,r1,r2,exs) ->
      let lo = gen_quotient_lens qc lc r1 r2 exs in
      begin match lo with
        | None -> failwith (n ^ " has no satisfying lens")
        | Some l ->
          (qc,rc,LensContext.insert_exn lc n l (kernel r1) (kernel r2),
          DeclQuotientLensCreation(n,r1,r2,l))
      end
    | DeclQuotientTestString (r, s) ->
        if fast_eval (QuotientRegexContext.to_whole_regex_context qc) (whole r) s then
          (qc, rc, lc, d)
        else
          failwith (s ^ " does not match regex " ^ (regex_to_string (whole r)))
    | DeclQuotientTestLens (n, exs) ->  
        List.iter
        ~f:(fun (lex,rex) ->
            let ans = lens_putr (QuotientRegexContext.to_kernel_regex_context qc) lc (LensVariable n) lex in
            if ans <> rex then
              failwith ("expected:" ^ rex ^ "got:" ^ ans)
            else
              ())
        exs;
      (qc,rc,lc,d)
  end

let synthesize_and_load_program
    (p:program)
  : QuotientRegexContext.t * RegexContext.t * LensContext.t * program =
  let (qc,rc,lc,p) = List.fold_left
      ~f:(fun (qc,rc,lc,p) d ->
          let (qc,rc,lc,d) = run_declaration qc rc lc d in
          (qc,rc,lc,d::p))
      ~init:(QuotientRegexContext.empty, RegexContext.empty, LensContext.empty, [])
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
  : QuotientRegexContext.t * RegexContext.t * LensContext.t =
  let (qc,rc,lc,_) = synthesize_and_load_program p in
  (qc,rc,lc)

let remove_tests : program -> program =
  List.filter
    ~f:(fun d ->
        begin match d with
          | DeclRegexCreation _ -> true
          | DeclTestString _ -> false
          | DeclSynthesizeLens _ -> true
          | DeclLensCreation _ -> false
          | DeclTestLens _ -> false
          | DeclQuotientRegexCreation _ -> true
          | DeclQuotientSynthesizeLens _ -> true
          | DeclQuotientLensCreation _ -> false
          | DeclQuotientTestString _ -> false
          | DeclQuotientTestLens _ -> false
        end)

let retrieve_last_synthesis_problem_exn
    (p:program)
  : QuotientRegexContext.t * RegexContext.t * LensContext.t * regex * regex * examples =
  let p = remove_tests p in
  let (qc,rc,lc,last_synth_option) = List.fold_left
    ~f:(fun (qc,rc,lc,last_synth_option) d ->
        begin match (d,last_synth_option) with
          | (DeclSynthesizeLens d,None) -> (qc,rc,lc,Some d)
          | (DeclSynthesizeLens spec',Some spec) ->
            let (qc,rc,lc,_) = run_declaration qc rc lc (DeclSynthesizeLens spec) in
            (qc,rc,lc, Some spec')
          | (_,_) ->
            let (qc,rc,lc,_) = run_declaration qc rc lc d in
            (qc,rc,lc,last_synth_option)
        end)
    ~init:(QuotientRegexContext.empty,RegexContext.empty,LensContext.empty,None)
    p
  in
  let (_,r1,r2,exs) = Option.value_exn last_synth_option in
  (qc,rc,lc,r1,r2,exs)
