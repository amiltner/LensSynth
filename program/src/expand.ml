open Core.Std
open Lenscontext
open Regexcontext
open Synth_structs
open Consts
open Util

let expand_once
    (rc:RegexContext.t)
    (qe:queue_element)
  : queue_element list =
  let rx_list =
    Transform.expand_once
      rc
      qe.r1
      qe.r2
  in

  List.map
    ~f:(fun (r1,r2) ->
        {
          r1 = r1;
          r2 = r2;
          expansions_performed = (qe.expansions_performed+1);
          expansions_inferred = qe.expansions_inferred;
          expansions_forced = qe.expansions_forced;
        })
    rx_list

let expand
    (rc:RegexContext.t)
    (lc:LensContext.t)
    (qe:queue_element)
  : queue_element list =
  if (!use_naive_expansion_search) then
    expand_once
      rc
      qe
  else
    let (r1,r2,exs) =
      Transform.expand_full_outermost_required_expansions
        rc
        lc
        qe.r1
        qe.r2
    in
    if (exs <> 0) then
      [{
        r1 = r1;
        r2 = r2;
        expansions_performed = (qe.expansions_performed+exs);
        expansions_inferred = (qe.expansions_inferred+exs);
        expansions_forced = (qe.expansions_forced+exs);
      }]
    else if !use_only_forced_expansions then
      expand_once
        rc
        qe
    else
      let s1 = Transform.get_full_current_level_user_defined_rep_set lc r1 in
      let s2 = Transform.get_full_current_level_user_defined_rep_set lc r2 in
      let problem_elements =
        (List.map
           ~f:(fun e -> Left e)
           (Transform.StringIntSet.as_list (Transform.StringIntSet.minus s1 s2)))
        @
        (List.map
           ~f:(fun e -> Right e)
           (Transform.StringIntSet.as_list (Transform.StringIntSet.minus s2 s1)))
      in
      if List.is_empty problem_elements then
        expand_once
          rc
          qe
      else
        let new_problems =
          List.concat_map
            ~f:(fun se ->
                begin match se with
                  | Left (v,star_depth) ->
                    let exposes = Transform.expose_full_userdef rc lc v star_depth r2 in
                    if List.is_empty exposes then
                      let (r1_expanded,expcount) =
                        Transform.force_expand_userdef
                          rc
                          lc
                          v
                          r1
                      in
                      [(r1_expanded,r2,expcount)]
                    else
                      List.map ~f:(fun (e,exp) -> (r1,e,exs+exp)) exposes
                  | Right (v,star_depth) ->
                    let exposes = Transform.expose_full_userdef rc lc v star_depth r1 in
                    if List.is_empty exposes then
                      failwith ("shoulda handled earlier  " ^ v ^ "   " ^ (string_of_int star_depth) ^ "\n\n" ^ (Pp.boom_pp_regex r1)
                                ^ "\n\n" ^ (Pp.boom_pp_regex r2))
                    else
                      List.map ~f:(fun (e,exp) -> (e,r2,exs+exp)) exposes
                end)
            problem_elements
        in
        List.map
          ~f:(fun (r1,r2,exp) ->
              {
                r1 = r1;
                r2 = r2;
                expansions_performed = (qe.expansions_performed+exp);
                expansions_inferred = (qe.expansions_inferred+exp);
                expansions_forced = qe.expansions_forced;
              })
          new_problems
