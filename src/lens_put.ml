open Core.Std
open Permutation
open Lang
open Fasteval
open Util
open Lens


let rec dnf_lens_putr (c:context)
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
        clause_lens_putr_internal atoms strings clause_lens iteration
    end

  and clause_lens_putr_internal
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
        ~f:(fun (exat,al) -> atom_lens_putr_internal exat al iteration)
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
      | (EAUserDefined (_,sl,ill), AIdentity) ->
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

  let e_d_r_o = regex_to_exampled_dnf_regex c r [s] in
  begin match e_d_r_o with
  | Some e_d_r ->
      let (e_cl_l,ill) = e_d_r in
      if ill <> [[0]] then
        failwith "bad ill"
      else
        dnf_lens_putr_internal e_cl_l l [0]
  | None -> failwith "string not in input"
  end

