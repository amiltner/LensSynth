open Core
open Regexcontext
open Lenscontext
open Lang
open Eval
open Normalized_lang
open Typing

let rec lens_putl_internal
    (rc:RegexContext.t)
    (lc:LensContext.t)
    (l:Lens.t)
    (er:exampled_regex)
    (iteration:int list)
  : string =
  begin match (l,er) with
    | (Lens.LensConst (s1,s2), ERegExBase (s2',_)) ->
        if s2 = s2' then
          s1
        else
          failwith "bad typecheck"
    | (Lens.LensVariable n, _) ->
      let relevant_string = extract_string er iteration in
      let limpl = LensContext.lookup_impl_exn lc n in
      let (_,limpl_rregex) = type_lens lc limpl in
      let er = Option.value_exn (regex_to_exampled_regex
          rc
          limpl_rregex
          [relevant_string])
      in
      lens_putl_internal rc lc limpl er [0]
    | (Lens.LensConcat (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_putl_internal rc lc l1 er1 iteration) ^
        (lens_putl_internal rc lc l2 er2 iteration)
    | (Lens.LensSwap (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_putl_internal rc lc l1 er2 iteration) ^
        (lens_putl_internal rc lc l2 er1 iteration)
    | (Lens.LensUnion (l1,l2), ERegExOr (er1,er2,_)) ->
        if took_regex er1 iteration then
          lens_putl_internal rc lc l1 er1 iteration
        else
          lens_putl_internal rc lc l2 er2 iteration
    | (Lens.LensCompose (l1,l2),_) ->
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
    | (Lens.LensIterate l', ERegExStar (er',_)) ->
        let valid_iterations =
          List.rev
            (List.filter
              ~f:(fun it -> List.tl_exn it = iteration)
              (extract_iterations_consumed er')) in
        String.concat
          (List.map
            ~f:(lens_putl_internal rc lc l' er')
            valid_iterations)
    | (Lens.LensIdentity _, _) ->
      extract_string er iteration
    | (Lens.LensInverse l', _) ->
      lens_putr_internal rc lc l' er iteration
    | _ -> failwith "bad typecheck"
  end

and lens_putr_internal
    (rc:RegexContext.t)
    (lc:LensContext.t)
    (l:Lens.t)
    (er:exampled_regex)
    (iteration:int list)
  : string =
  begin match (l,er) with
    | (Lens.LensConst (s1,s2), ERegExBase (s1',_)) ->
        if s1 = s1' then
          s2
        else
          failwith "bad typecheck"
    | (Lens.LensVariable n, _) ->
      let relevant_string = extract_string er iteration in
      let limpl = LensContext.lookup_impl_exn lc n in
      let (limpl_lregex,_) = type_lens lc limpl in
      let er = Option.value_exn (regex_to_exampled_regex
          rc
          limpl_lregex
          [relevant_string])
      in
      lens_putr_internal rc lc limpl er [0]
    | (Lens.LensConcat (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_putr_internal rc lc l1 er1 iteration) ^
        (lens_putr_internal rc lc l2 er2 iteration)
    | (Lens.LensSwap (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_putr_internal rc lc l2 er2 iteration) ^
        (lens_putr_internal rc lc l1 er1 iteration)
    | (Lens.LensUnion (l1,l2), ERegExOr (er1,er2,_)) ->
        if took_regex er1 iteration then
          lens_putr_internal rc lc l1 er1 iteration
        else
          lens_putr_internal rc lc l2 er2 iteration
    | (Lens.LensCompose (l1,l2),_) ->
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
    | (Lens.LensIterate l', ERegExStar (er',_)) ->
        let valid_iterations =
          List.rev
            (List.filter
              ~f:(fun it -> List.tl_exn it = iteration)
              (extract_iterations_consumed er')) in
        String.concat
          (List.map
            ~f:(lens_putr_internal rc lc l' er')
            valid_iterations)
    | (Lens.LensIdentity _, _) ->
      extract_string er iteration
    | (Lens.LensInverse l', _) ->
      lens_putl_internal rc lc l' er iteration
    | _ -> failwith "bad typecheck"
  end

let lens_putr (rc:RegexContext.t)
    (lc:LensContext.t)
    (l:Lens.t)
    (s:string)
  : string =
  let (sr,_) = type_lens lc l in
  let exampled_sr_o = regex_to_exampled_regex rc sr [s] in
  begin match exampled_sr_o with
    | None -> failwith ("bad input to lens" ^ s)
    | Some exampled_sr -> lens_putr_internal rc lc l exampled_sr [0]
  end

let lens_putl (rc:RegexContext.t)
    (lc:LensContext.t)
    (l:Lens.t)
    (s:string)
  : string =
  let (_,tr) = type_lens lc l in
  let exampled_sr_o = regex_to_exampled_regex rc tr [s] in
  begin match exampled_sr_o with
    | None -> failwith ("bad input to lens: " ^ s)
    | Some exampled_sr -> lens_putl_internal rc lc l exampled_sr [0]
  end
