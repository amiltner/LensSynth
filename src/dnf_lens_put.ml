open Core.Std
open Lenscontext
open Permutation
open Normalized_lang
open Regex
open Regexcontext
open Lang
open Eval
open Util
open Lens
open Converter
open Lens_put

let rec dnf_lens_putr
    (rc:RegexContext.t)
    (lc:LensContext.t)
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
      | (EAStar ((cll,ill),_),AtomLensIterate dl) ->
          let valid_ils =
            List.rev
              (List.filter
                ~f:(fun il' -> List.tl_exn il' = iteration)
                ill) in
          String.concat
            (List.map
              ~f:(dnf_lens_putr_internal cll dl)
              valid_ils)
      | (EAVariable (name,name_orig,lens_between,sl,ill), AtomLensVariable l) ->
          let dat_opt = List.findi
            ~f:(fun i il -> il = iteration)
            ill in
          begin match dat_opt with
          | None -> failwith "im horrible"
          | Some (i,_) ->
            let relevant_str = List.nth_exn sl i in
            let original_relevant_str = lens_putl rc lc lens_between relevant_str in
            lens_putr rc lc l original_relevant_str
          end
      | _ -> failwith "stupid bad stupid bad"
      end
  in

  let mcsleftside = get_left_side mcs in

  let e_d_r_o = regex_to_exampled_dnf_regex rc LensContext.empty mcsleftside r [s] in
  begin match e_d_r_o with
  | Some e_d_r ->
      let (e_cl_l,ill) = e_d_r in
      if ill <> [[0]] then
        failwith "bad ill"
      else
        dnf_lens_putr_internal e_cl_l l [0]
  | None -> failwith "string not in input"
  end
