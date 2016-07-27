open Core.Std
open Permutation
open Regexcontext
open Lang
open Fasteval
open Util
open Lens


let rec lens_putl_internal
    (rc:RegexContext.t)
    (l:lens)
    (er:exampled_regex)
    (iteration:int list)
  : string =
  begin match (l,er) with
    | (ConstLens (s1,s2), ERegExBase (s2',_)) ->
        if s2 = s2' then
          s1
        else
          failwith "bad typecheck"
    | (ConcatLens (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_putl_internal rc l1 er1 iteration) ^
        (lens_putl_internal rc l2 er2 iteration)
    | (SwapLens (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_putl_internal rc l1 er2 iteration) ^
        (lens_putl_internal rc l2 er1 iteration)
    | (UnionLens (l1,l2), ERegExOr (er1,er2,_)) ->
        if took_regex er1 iteration then
          lens_putl_internal rc l1 er1 iteration
        else
          lens_putl_internal rc l2 er2 iteration
    | (ComposeLens (l1,l2),_) ->
      let intermediary_string = lens_putl_internal rc l1 er iteration in
      let (_,intermediary_regex) = type_lens l2 in
      let intermediary_er_o = regex_to_exampled_regex
          rc
          []
          intermediary_regex
          [intermediary_string]
      in
      begin match intermediary_er_o with
        | None -> failwith "bad input to lens"
        | Some intermediary_er -> lens_putl_internal rc l2 intermediary_er [0]
      end
    | (IterateLens l', ERegExStar (er',_)) ->
        let valid_iterations =
          List.rev
            (List.filter
              ~f:(fun it -> List.tl_exn it = iteration)
              (extract_iterations_consumed er')) in
        String.concat
          (List.map
            ~f:(lens_putl_internal rc l' er')
            valid_iterations)
    | (IdentityLens _, _) ->
      extract_string er iteration
    | (InverseLens l', _) ->
      lens_putr_internal rc l' er iteration
    | _ -> failwith "bad typecheck"
  end

and lens_putr_internal
    (rc:RegexContext.t)
    (l:lens)
    (er:exampled_regex)
    (iteration:int list)
  : string =
  begin match (l,er) with
    | (ConstLens (s1,s2), ERegExBase (s1',_)) ->
        if s1 = s1' then
          s2
        else
          failwith "bad typecheck"
    | (ConcatLens (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_putr_internal rc l1 er1 iteration) ^
        (lens_putr_internal rc l2 er2 iteration)
    | (SwapLens (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_putr_internal rc l2 er2 iteration) ^
        (lens_putr_internal rc l1 er1 iteration)
    | (UnionLens (l1,l2), ERegExOr (er1,er2,_)) ->
        if took_regex er1 iteration then
          lens_putr_internal rc l1 er1 iteration
        else
          lens_putr_internal rc l2 er2 iteration
    | (ComposeLens (l1,l2),_) ->
      let intermediary_string = lens_putr_internal rc l2 er iteration in
      let (intermediary_regex,_) = type_lens l1 in
      let intermediary_er_o = regex_to_exampled_regex
          rc
          []
          intermediary_regex
          [intermediary_string]
      in
      begin match intermediary_er_o with
        | None -> failwith "bad input to lens"
        | Some intermediary_er -> lens_putr_internal rc l1 intermediary_er [0]
      end
    | (IterateLens l', ERegExStar (er',_)) ->
        let valid_iterations =
          List.rev
            (List.filter
              ~f:(fun it -> List.tl_exn it = iteration)
              (extract_iterations_consumed er')) in
        String.concat
          (List.map
            ~f:(lens_putr_internal rc l' er')
            valid_iterations)
    | (IdentityLens _, _) ->
      extract_string er iteration
    | (InverseLens l', _) ->
      lens_putl_internal rc l' er iteration
    | _ -> failwith "bad typecheck"
  end

let lens_putr (rc:RegexContext.t)
    (l:lens)
    (s:string)
  : string =
  let (sr,_) = type_lens l in
  let exampled_sr_o = regex_to_exampled_regex rc [] sr [s] in
  begin match exampled_sr_o with
    | None -> failwith "bad input to lens"
    | Some exampled_sr -> lens_putr_internal rc l exampled_sr [0]
  end

let lens_putl (rc:RegexContext.t)
    (l:lens)
    (s:string)
  : string =
  let (_,tr) = type_lens l in
  let exampled_sr_o = regex_to_exampled_regex rc [] tr [s] in
  begin match exampled_sr_o with
    | None -> failwith "bad input to lens"
    | Some exampled_sr -> lens_putl_internal rc l exampled_sr [0]
  end

let rec dnf_lens_putr (c:RegexContext.t)
                      (mcs:mapsbetweencontext)
                      (r:regex)
                      (l:dnf_lens)
                      (s:string)
                      : string =
  let rec dnf_lens_putr_internal (cs:exampled_clause list)
                                 ((clause_lenses,_):dnf_lens)
                                 (iteration:int list)
                                 : string =
    let dat_opt = List.findi
      ~f:(fun _ (_,_,ill) -> (List.mem ill iteration))
      cs in
    begin match dat_opt with
    | None -> failwith "bad doesnt exist bad bad"
    | Some (i,(atoms,strings,_)) ->
        let clause_lens = List.nth_exn clause_lenses i in
        clause_lens_putr_internal mcs atoms strings clause_lens iteration
    end

  and clause_lens_putr_internal
    (mcs:mapsbetweencontext)
    (atoms:exampled_atom list)
    (strings:string list)
    ((all,p,sl1,sl2):clause_lens)
    (iteration:int list)
    : string =
      let exat_al_zips =
        List.zip_exn
          atoms
          all
      in
      let rstrs = List.map
        ~f:(fun (exat,al) -> atom_lens_putr_internal mcs exat al iteration)
        exat_al_zips
      in
      let rstrs_permuted = Permutation.apply_to_list_exn p rstrs in
      let (sl2h,sl2t) = split_by_first_exn sl2 in
      let rsp_sl2t_zipped = List.zip_exn
        rstrs_permuted
        sl2t
      in
      sl2h ^
      String.concat (List.map ~f:(fun (s1,s2) -> s1 ^ s2) rsp_sl2t_zipped)

  and atom_lens_putr_internal
    (mcs:mapsbetweencontext)
    (a:exampled_atom)
    (al:atom_lens)
    (iteration:int list)
    : string =
      begin match (a,al) with
      | (EAStar ((cll,ill),_),AIterate dl) ->
          let valid_ils =
            List.rev
              (List.filter
                ~f:(fun il' -> List.tl_exn il' = iteration)
                ill) in
          String.concat
            (List.map
              ~f:(dnf_lens_putr_internal cll dl)
              valid_ils)
      | (EAUserDefined (_,sl,ill), AIdentity _) ->
          let dat_opt = List.findi
            ~f:(fun i il -> il = iteration)
            ill in
          begin match dat_opt with
          | None -> failwith "im horrible"
          | Some (i,_) ->
              List.nth_exn sl i
          end
      | _ -> failwith "stupid bad stupid bad"
      end
  in

  let mcsleftside = get_left_side mcs in

  let e_d_r_o = regex_to_exampled_dnf_regex c mcsleftside r [s] in
  begin match e_d_r_o with
  | Some e_d_r ->
      let (e_cl_l,ill) = e_d_r in
      if ill <> [[0]] then
        failwith "bad ill"
      else
        dnf_lens_putr_internal e_cl_l l [0]
  | None -> failwith "string not in input"
  end

