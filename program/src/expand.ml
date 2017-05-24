open Core
open Lang
open Lenscontext
open Regexcontext
open Synth_structs
open Consts
open Util
open String_utilities

(**** GetSets {{{ *****)
module IdSet = Comparison_set.Make(
  struct
    type element = id
    let compare = compare_id
    let to_string = show_id
  end)

module IntSet = Comparison_set.Make(
  struct
    type element = int
    let compare = compare_int
    let to_string = string_of_int
  end)

module IdIntSet = Comparison_set.Make(
  struct
    type element = id * int
    let compare = pair_compare compare_id compare_int
    let to_string = string_of_pair show_id string_of_int
  end)

module IdToIntSetDict = Dict.Make(
  struct
    type key = id
    type value = IntSet.set
    let compare_key = compare_id
    let compare_value = IntSet.compare
    let key_to_string = show_id
    let value_to_string = IntSet.to_string
  end)

let get_rep_userdef
    (lc:LensContext.t)
    (ud:id)
  : id =
  if !use_lens_context then
    (fst (LensContext.shortest_path_to_rep_elt lc ud))
  else
    ud

let get_current_set
    (lc:LensContext.t)
    (r:Regex.t)
  : IdIntSet.set =
  let rec get_current_level_user_defined_rep_set_internal
      (lc:LensContext.t)
      (r:Regex.t)
      (depth:int)
    : IdIntSet.set =
    begin match r with
      | Regex.RegExEmpty -> IdIntSet.empty
      | Regex.RegExBase _ -> IdIntSet.empty
      | Regex.RegExConcat (r1,r2) ->
        let s1 =
          get_current_level_user_defined_rep_set_internal
            lc
            r1
            depth
        in
        let s2 =
          get_current_level_user_defined_rep_set_internal
            lc
            r2
            depth
        in
        IdIntSet.union s1 s2
      | Regex.RegExOr (r1,r2) ->
        let s1 =
          get_current_level_user_defined_rep_set_internal
            lc
            r1
            depth
        in
        let s2 =
          get_current_level_user_defined_rep_set_internal
            lc
            r2
            depth
        in
        IdIntSet.union s1 s2
      | Regex.RegExStar r' ->
        get_current_level_user_defined_rep_set_internal
          lc
          r'
          (depth+1)
      | Regex.RegExVariable v ->
        let v' = get_rep_userdef lc v in
        IdIntSet.singleton
          (v',depth)
    end
  in
  get_current_level_user_defined_rep_set_internal
    lc
    r
    0
(***** }}} *****)



(**** ForceExpand {{{ *****)
(***** }}} *****)




(**** Reveal {{{ *****)
let rec reveal
    (rc:RegexContext.t)
    (lc:LensContext.t)
    (v:id)
    (star_depth:int)
    (r:Regex.t)
  : (Regex.t * int) list =
  begin match r with
    | Regex.RegExVariable v' ->
      if get_rep_userdef lc v' = v && star_depth = 0 then
        [(Regex.RegExVariable v',0)]
      else
        begin match RegexContext.lookup_for_expansion_exn rc v' with
          | None -> []
          | Some r' ->
            List.map
              ~f:(fun (r,exp) -> (r,exp+1))
              (reveal
                 rc
                 lc
                 v
                 star_depth
                 r')
        end
    | Regex.RegExConcat (r1,r2) ->
      let r1_exposes = reveal rc lc v star_depth r1 in
      let r2_exposes = reveal rc lc v star_depth r2 in
      (List.map
         ~f:(fun (r1e,exp) -> (Regex.RegExConcat (r1e,r2),exp))
         r1_exposes)
      @
      (List.map
         ~f:(fun (r2e,exp) -> (Regex.RegExConcat (r1,r2e),exp))
         r2_exposes)
    | Regex.RegExOr (r1,r2) ->
      let r1_exposes = reveal rc lc v star_depth r1 in
      let r2_exposes = reveal rc lc v star_depth r2 in
      (List.map
         ~f:(fun (r1e,exp) -> (Regex.RegExOr (r1e,r2),exp))
         r1_exposes)
      @
      (List.map
         ~f:(fun (r2e,exp) -> (Regex.RegExOr (r1,r2e),exp))
         r2_exposes)
    | Regex.RegExStar r' ->
      let r'_exposes_with_unfold =
        reveal
          rc
          lc
          v
          star_depth
          r'
      in
      let r'_exposes_underneath =
        reveal
          rc
          lc
          v
          (star_depth-1)
          r'
      in
      let star_r'_exposes_underneath =
        (List.map
           ~f:(fun (r'e,exp) -> (Regex.RegExStar r'e,exp))
           r'_exposes_underneath)
      in
      let unrolled_r'_exposes_left =
        (List.map
           ~f:(fun (r'e,exp) -> (Regex.RegExOr(Regex.RegExBase "", Regex.RegExConcat (r'e, Regex.RegExStar r'e)) ,exp+1))
           r'_exposes_with_unfold)
      in
      let unrolled_r'_exposes_right =
        (List.map
           ~f:(fun (r'e,exp) -> (Regex.RegExOr(Regex.RegExBase "", Regex.RegExConcat (Regex.RegExStar r'e, r'e)) ,exp+1))
           r'_exposes_with_unfold)
      in
      star_r'_exposes_underneath@unrolled_r'_exposes_left@unrolled_r'_exposes_right
    | Regex.RegExEmpty -> []
    | Regex.RegExBase _ -> []
  end
(***** }}} *****)

(**** ExpandOnce {{{ *****)
let expand_once
    (rc:RegexContext.t)
    (qe:queue_element)
  : queue_element list =
  let expanders =
    [Algebra.left_unfold_all_stars regex_star_semiring
    ;Algebra.right_unfold_all_stars regex_star_semiring
    ;Regex.applies_for_every_applicable_level
        (fun r ->
           option_bind
             ~f:(RegexContext.lookup_for_expansion_exn rc)
             (Regex.separate_userdef r))]
  in

  let retrieve_new_problems_from_expander
      (transform:Regex.t -> Regex.t list)
    : (Regex.t * Regex.t) list =
    (List.map
       ~f:(fun le -> (le, qe.r2))
       (transform qe.r1))
    @
    (List.map
       ~f:(fun re -> (qe.r1, re))
       (transform qe.r2))
  in

  let new_problems =
    List.concat_map
      ~f:retrieve_new_problems_from_expander
      expanders
  in

  let new_queue_elements =
    List.map
      ~f:(fun (r1,r2) ->
          {
            r1 = r1;
            r2 = r2;
            expansions_performed = (qe.expansions_performed+1);
            expansions_inferred = qe.expansions_inferred;
            expansions_forced = qe.expansions_forced;
          })
      new_problems
  in

  new_queue_elements
(***** }}} *****)


(**** ExpandRequired {{{ *****)
let expand_required
    (rc:RegexContext.t)
    (lc:LensContext.t)
    (r1:Regex.t)
    (r2:Regex.t)
  : (Regex.t * Regex.t * int) =
  let expand_full_outermost_required_expansions_internal
      (rc:RegexContext.t)
      (lc:LensContext.t)
      ((r1,r2,exps):Regex.t * Regex.t * int)
    : Regex.t * Regex.t * int =
    let rec retrieve_full_transitive_userdefs
        (r:Regex.t)
        (star_depth:int)
      : (id * int) list =
      begin match r with
        | Regex.RegExEmpty -> []
        | Regex.RegExBase _ -> []
        | Regex.RegExConcat (r1,r2) -> (retrieve_full_transitive_userdefs r1 star_depth) @
                                 (retrieve_full_transitive_userdefs r2 star_depth)
        | Regex.RegExOr (r1,r2) -> (retrieve_full_transitive_userdefs r1 star_depth) @
                             (retrieve_full_transitive_userdefs r2 star_depth)
        | Regex.RegExStar r' -> retrieve_full_transitive_userdefs r' (star_depth+1)
        | Regex.RegExVariable t -> (get_rep_userdef lc t, star_depth)::
                             (begin match RegexContext.lookup_for_expansion_exn rc t with
                                | None -> []
                                | Some rex -> retrieve_full_transitive_userdefs rex star_depth
                              end)
      end
    in
    let rec expand_full_required_expansions (bad_userdefs:(id * int) list)
        (r:Regex.t)
        (star_depth:int)
      : Regex.t * int =
      begin match r with
        | Regex.RegExEmpty -> (r,0)
        | Regex.RegExBase _ -> (r,0)
        | Regex.RegExConcat (r1,r2) ->
          let (r1',e1) = expand_full_required_expansions bad_userdefs r1 star_depth in
          let (r2',e2) = expand_full_required_expansions bad_userdefs r2 star_depth in
          (Regex.RegExConcat(r1',r2'),e1+e2)
        | Regex.RegExOr (r1,r2) -> 
          let (r1',e1) = expand_full_required_expansions bad_userdefs r1 star_depth in
          let (r2',e2) = expand_full_required_expansions bad_userdefs r2 star_depth in
          (Regex.RegExOr(r1',r2'),e1+e2)
        | Regex.RegExStar r' ->
          let (r'',e') = expand_full_required_expansions bad_userdefs r' (star_depth+1) in
          (Regex.RegExStar(r''),e')
        | Regex.RegExVariable t ->
          if List.mem ~equal:(=) bad_userdefs (t,star_depth) then
            begin match RegexContext.lookup_for_expansion_exn rc
                          (get_rep_userdef lc t) with
            | None -> failwith "no solution"
            | Some r' ->
              let (r'',e') = expand_full_required_expansions bad_userdefs r' star_depth in
              (r'',e'+1)
            end
          else
            (r,0)
      end
    in

    let r1_transitive_userdefs = retrieve_full_transitive_userdefs r1 0 in
    let r2_transitive_userdefs = retrieve_full_transitive_userdefs r2 0 in
    let all_userdefs =
      IdSet.from_list
        ((List.map ~f:fst r1_transitive_userdefs)
         @(List.map ~f:fst r2_transitive_userdefs))
    in
    let empty_depths_dict =
      List.fold_left
        ~f:(fun d v ->
            IdToIntSetDict.insert d v IntSet.empty)
        ~init:IdToIntSetDict.empty
        (IdSet.as_list all_userdefs)
    in
    let r1_depths =
      List.fold_left
        ~f:(fun d (v,depth) ->
            let previous_depths = IdToIntSetDict.lookup_exn d v in
            IdToIntSetDict.insert d v (IntSet.insert depth previous_depths))
        ~init:empty_depths_dict
        r1_transitive_userdefs
    in
    let r2_depths =
      List.fold_left
        ~f:(fun d (v,depth) ->
            let previous_depths = IdToIntSetDict.lookup_exn d v in
            IdToIntSetDict.insert d v (IntSet.insert depth previous_depths))
        ~init:empty_depths_dict
        r2_transitive_userdefs
    in

    let unreachables =
      List.fold_left
        ~f:(fun acc v ->
            let r1_depth_list =
              IntSet.as_list
                (IdToIntSetDict.lookup_exn
                   r1_depths
                   v)
            in
            let r2_depth_list =
              IntSet.as_list
                (IdToIntSetDict.lookup_exn
                   r2_depths
                   v)
            in
            let not_accessible_in_r1_list =
              List.filter
                ~f:(fun d ->
                    List.for_all ~f:(fun d' -> d > d') r2_depth_list)
                r1_depth_list
            in
            let not_accessible_in_r2_list =
              List.filter
                ~f:(fun d ->
                    List.for_all ~f:(fun d' -> d > d') r1_depth_list)
                r2_depth_list
            in
            (List.map
               ~f:(fun d -> Left (v,d))
               not_accessible_in_r1_list)
            @
            (List.map
               ~f:(fun d -> Right (v,d))
               not_accessible_in_r2_list)
            @
            acc)
        ~init:[]
        (IdSet.as_list all_userdefs)
    in
    let (left_unreachables,right_unreachables) = split_by_either unreachables in
    begin match (expand_full_required_expansions left_unreachables r1 0,
                 expand_full_required_expansions right_unreachables r2 0) with
    | ((r1',e1),(r2',e2)) -> (r1',r2',e1+e2+exps)
    end
  in
  fold_until_fixpoint
    (expand_full_outermost_required_expansions_internal
       rc
       lc)
    (r1,r2,0)
(***** }}} *****)


(**** FixProblemElts {{{ *****)
let fix_problem_elts
    (rc:RegexContext.t)
    (lc:LensContext.t)
    (qe:queue_element)
  : queue_element list =
  let s1 = get_current_set lc qe.r1 in
  let s2 = get_current_set lc qe.r2 in
  let problem_elements =
    (List.map
       ~f:(fun e -> Left e)
       (IdIntSet.as_list (IdIntSet.minus s1 s2)))
    @
    (List.map
       ~f:(fun e -> Right e)
       (IdIntSet.as_list (IdIntSet.minus s2 s1)))
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
                let exposes = reveal rc lc v star_depth qe.r2 in
                if List.is_empty exposes then
                  failwith ("shoulda handled earlier  " ^ (show_id v) ^ "   " ^ (string_of_int star_depth) ^ "\n\n" ^ (Pp.boom_pp_regex qe.r1)
                            ^ "\n\n" ^ (Pp.boom_pp_regex qe.r2))
                else
                  List.map ~f:(fun (e,exp) -> (qe.r1,e,exp)) exposes
              | Right (v,star_depth) ->
                let exposes = reveal rc lc v star_depth qe.r1 in
                if List.is_empty exposes then
                  failwith ("shoulda handled earlier  " ^ (show_id v) ^ "   " ^ (string_of_int star_depth) ^ "\n\n" ^ (Pp.boom_pp_regex qe.r1)
                            ^ "\n\n" ^ (Pp.boom_pp_regex qe.r2))
                else
                  List.map ~f:(fun (e,exp) -> (e,qe.r2,exp)) exposes
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
(***** }}} *****)

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
      expand_required
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
      fix_problem_elts
        rc
        lc
        qe
