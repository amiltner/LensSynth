open Core.Std
open Regexcontext
open Lenscontext
open Lang
open Eval
open Qre_context
open Quotient_regex
open Normalized_lang
open Typing
open Permutation

let rec lens_putl_internal
    (rc:RegexContext.t)
    (lc:LensContext.t)
    (l:lens)
    (er:exampled_regex)
    (iteration:int list)
  : string =
  begin match (l,er) with
    | (LensConst (s1,s2), ERegExBase (s2',_)) ->
        if s2 = s2' then
          s1
        else
          failwith "bad typecheck"
    | (LensVariable n, _) ->
      let relevant_string = extract_string er iteration in
      let limpl = LensContext.lookup_impl_exn lc n in
      let (_,limpl_rregex) = type_lens lc limpl in
      let er = Option.value_exn (regex_to_exampled_regex
          rc
          limpl_rregex
          [relevant_string])
      in
      lens_putl_internal rc lc limpl er [0]
    | (LensConcat (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_putl_internal rc lc l1 er1 iteration) ^
        (lens_putl_internal rc lc l2 er2 iteration)
    | (LensSwap (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_putl_internal rc lc l1 er2 iteration) ^
        (lens_putl_internal rc lc l2 er1 iteration)
    | (LensUnion (l1,l2), ERegExOr (er1,er2,_)) ->
        if took_regex er1 iteration then
          lens_putl_internal rc lc l1 er1 iteration
        else
          lens_putl_internal rc lc l2 er2 iteration
    | (LensCompose (l1,l2),_) ->
      let intermediary_string = lens_putl_internal rc lc l1 er iteration in
      let (_,intermediary_regex) = type_lens lc l2 in
      let intermediary_er_o = regex_to_exampled_regex
          rc
          intermediary_regex
          [intermediary_string]
      in
      begin match intermediary_er_o with
        | None -> failwith "bad input to lens"
        | Some intermediary_er -> lens_putl_internal rc lc l2 intermediary_er [0]
      end
    | (LensIterate l', ERegExStar (er',_)) ->
        let valid_iterations =
          List.rev
            (List.filter
              ~f:(fun it -> List.tl_exn it = iteration)
              (extract_iterations_consumed er')) in
        String.concat
          (List.map
            ~f:(lens_putl_internal rc lc l' er')
            valid_iterations)
    | (LensIdentity _, _) ->
      extract_string er iteration
    | (LensInverse l', _) ->
      lens_putr_internal rc lc l' er iteration
    | _ -> failwith "bad typecheck"
  end

and lens_putr_internal
    (rc:RegexContext.t)
    (lc:LensContext.t)
    (l:lens)
    (er:exampled_regex)
    (iteration:int list)
  : string =
  begin match (l,er) with
    | (LensConst (s1,s2), ERegExBase (s1',_)) ->
        if s1 = s1' then
          s2
        else
          failwith "bad typecheck"
    | (LensVariable n, _) ->
      let relevant_string = extract_string er iteration in
      let limpl = LensContext.lookup_impl_exn lc n in
      let (limpl_lregex,_) = type_lens lc limpl in
      let er = Option.value_exn (regex_to_exampled_regex
          rc
          limpl_lregex
          [relevant_string])
      in
      lens_putr_internal rc lc limpl er [0]
    | (LensConcat (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_putr_internal rc lc l1 er1 iteration) ^
        (lens_putr_internal rc lc l2 er2 iteration)
    | (LensSwap (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_putr_internal rc lc l2 er2 iteration) ^
        (lens_putr_internal rc lc l1 er1 iteration)
    | (LensUnion (l1,l2), ERegExOr (er1,er2,_)) ->
        if took_regex er1 iteration then
          lens_putr_internal rc lc l1 er1 iteration
        else
          lens_putr_internal rc lc l2 er2 iteration
    | (LensCompose (l1,l2),_) ->
      let intermediary_string = lens_putr_internal rc lc l2 er iteration in
      let (intermediary_regex,_) = type_lens lc l1 in
      let intermediary_er_o = regex_to_exampled_regex
          rc
          intermediary_regex
          [intermediary_string]
      in
      begin match intermediary_er_o with
        | None -> failwith "bad input to lens"
        | Some intermediary_er -> lens_putr_internal rc lc l1 intermediary_er [0]
      end
    | (LensIterate l', ERegExStar (er',_)) ->
        let valid_iterations =
          List.rev
            (List.filter
              ~f:(fun it -> List.tl_exn it = iteration)
              (extract_iterations_consumed er')) in
        String.concat
          (List.map
            ~f:(lens_putr_internal rc lc l' er')
            valid_iterations)
    | (LensIdentity _, _) ->
      extract_string er iteration
    | (LensInverse l', _) ->
      lens_putl_internal rc lc l' er iteration
    | _ -> failwith "bad typecheck"
  end

let lens_putr (rc:RegexContext.t)
    (lc:LensContext.t)
    (l:lens)
    (s:string)
  : string =
  let (sr,_) = type_lens lc l in
  let exampled_sr_o = regex_to_exampled_regex rc sr [s] in
  begin match exampled_sr_o with
    | None -> failwith "bad input to lens"
    | Some exampled_sr -> lens_putr_internal rc lc l exampled_sr [0]
  end

let lens_putl (rc:RegexContext.t)
    (lc:LensContext.t)
    (l:lens)
    (s:string)
  : string =
  let (_,tr) = type_lens lc l in
  let exampled_sr_o = regex_to_exampled_regex rc tr [s] in
  begin match exampled_sr_o with
    | None -> failwith "bad input to lens"
    | Some exampled_sr -> lens_putl_internal rc lc l exampled_sr [0]
  end

let fact x =
  let rec fact' acc x =
    if x = 0 then acc else fact' (x * acc) (x - 1) in
  fact' 1 x

(* Assuming e at the toplevel is a nested OR, pick out the regex that we matched. *)
let rec get_matched_or e it index n =
  match e with
  | ERegExOr (e1, e2, _) ->
      if took_regex e1 it then e1, index
      else get_matched_or e2 it (index + 1) n
  | _ -> if index = fact n - 1 then (e, index)
         else failwith "Failure to typecheck" (* This means we got to the end *)

(* Permutation gives a series s1 sep s2 .. sep sn. Here we split s_i from the separators. *)
let rec get_elems_and_seps e n =
  match e, n with
  | ERegExConcat (e1, ERegExConcat (e2, rest, _), _), n ->
      let (elems, seps) = get_elems_and_seps rest (n - 1) in
      e1 :: elems, e2 :: seps
  | x, 0 -> ([x], [])
  | _ -> failwith ("Failure to typecheck " ^ (exampled_regex_to_string e) ^ (string_of_int n))


let rec quotient_lens_canonize_internal qc ql er it : string =
  match (ql, er) with
  | QuotientRegExBase s, _ -> s
  | QuotientRegExMap (_, s), _ -> s
  | QuotientRegExConcat (q1, q2), ERegExConcat (e1, e2, _) ->
      (quotient_lens_canonize_internal qc q1 e1 it) ^
      (quotient_lens_canonize_internal qc q2 e2 it)
  | QuotientRegExOr (q1, q2), ERegExOr (e1, e2, _) ->
      if took_regex e1 it then
        quotient_lens_canonize_internal qc q1 e1 it
      else
        quotient_lens_canonize_internal qc q2 e2 it
  | QuotientRegExStar q, ERegExStar (e, _) ->
      let valid_iterations =
        List.rev
          (List.filter
            ~f:(fun iter -> List.tl_exn iter = it)
            (extract_iterations_consumed e)) in
      String.concat 
        (List.map 
          ~f:(quotient_lens_canonize_internal qc q e)
          valid_iterations)
  | QuotientRegExPermute (l, _), r ->
    let (matched_or, perm_index) = get_matched_or r it 0 (List.length l) in
    let permutation = List.nth_exn (Permutation.create_all (List.length l)) (fact (List.length l) - 1 - perm_index) in
    (* We need to subtract here because in the construction of the whole it's in the opposite order *)
    let (elems, seps) = get_elems_and_seps matched_or (List.length l - 1) in
    let ordered_elems = Permutation.apply_inverse_to_list_exn permutation elems in
    permutation_to_string l ordered_elems seps qc it
  | QuotientRegExVariable s, _ ->
      let relevant_string = extract_string er it in 
      let qre = QuotientRegexContext.lookup_exn qc s in
      let rc_of_qc = QuotientRegexContext.to_whole_regex_context qc in
      let exampled = Option.value_exn (regex_to_exampled_regex rc_of_qc (whole qre) [relevant_string]) in
      quotient_lens_canonize_internal qc qre exampled [0]
  | _ -> failwith "Failure to typecheck"

and permutation_to_string l elems seps qc it : string =
  match l, elems, seps with
  | [], _, _ -> ""
  | [h], [e], [] -> quotient_lens_canonize_internal qc h e it
  | h :: t, e :: es, sep :: seps ->
      (quotient_lens_canonize_internal qc h e it) ^
      (extract_string sep it) ^
      (permutation_to_string t es seps qc it)
  | _ -> failwith "Failure to typecheck"
  
(* This should work since the kernel always is contained in the whole*)
let quotient_lens_choose (_ : RegexContext.t) 
    (_ : QuotientRegexContext.t) 
    (_ : quotient_regex) 
    (s : string) 
  : string = s

let quotient_lens_canonize qc q s =
  let exampled_sr_o = regex_to_exampled_regex (QuotientRegexContext.to_whole_regex_context qc) (whole q) [s] in
  begin match exampled_sr_o with
    | None -> failwith "bad input to lens"
    | Some exampled_sr -> quotient_lens_canonize_internal qc q exampled_sr [0]
  end
