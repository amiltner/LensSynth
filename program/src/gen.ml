open Core.Std
open Lenscontext
open Converter
open Regexcontext
open Lang
open Lens_utilities
open Util
open Permutation
open Transform
open Normalized_lang
open Consts
open Naive_gen
open String_utilities

type new_queue_element = regex * regex * int * int * int * int

let new_queue_element_comparison =
  sext_compare
    regex_compare
    regex_compare
    (fun _ _ -> EQ)
    (fun _ _ -> EQ)
    (fun _ _ -> EQ)
    (fun _ _ -> EQ)

let new_queue_element_to_string =
  string_of_sextuple
    regex_to_string
    regex_to_string
    string_of_int
    string_of_int
    string_of_int
    string_of_int

module UDEF_DISTANCE_PQUEUE = Priority_queue_two.Make(
  struct
    type element = new_queue_element
    let compare = new_queue_element_comparison

    let priority
        ((_,_,d,exps_performed,_,_) : new_queue_element)
      : int =
      retrieve_priority
        d
        exps_performed

    let to_string = new_queue_element_to_string
  end)

module EXPANDCOUNT_PQUEUE = Priority_queue_two.Make(
  struct
    type element = new_queue_element
    let compare = new_queue_element_comparison

    let priority
        ((_,_,_,exps_performed,_,_) : new_queue_element)
      : int =
      exps_performed

    let to_string = new_queue_element_to_string
  end)

module type LENS_SYNTHESIZER =
sig
  val gen_lens : RegexContext.t -> LensContext.t -> regex -> regex -> examples -> lens option
end

module type LENSSYNTH_PRIORITY_QUEUE =
sig
  type queue
  type element = new_queue_element

  val empty : queue
  val from_list : element list -> queue
  val push : queue -> element -> queue
  val push_all : queue -> element list -> queue
  val pop : queue -> (element * int * queue) option
  val length : queue -> int
  val compare : queue -> queue -> comparison
  val to_string : queue -> string
end

module DNFSynth(PQ : LENSSYNTH_PRIORITY_QUEUE) =
struct
  type synthesis_info =
    {
      l                    : dnf_lens ;
      specs_visited        : int      ;
      expansions_performed : int      ;
      expansions_inferred  : int      ;
      expansions_forced    : int      ;
    }

  let rec gen_atom_zipper (lc:LensContext.t)
      (atom1:ordered_exampled_atom)
      (atom2:ordered_exampled_atom)
    : atom_lens =
    begin match (atom1,atom2) with
      | (OEAUserDefined (_,sorig1,_,_),OEAUserDefined (_,sorig2,_,_)) ->
        AtomLensVariable (LensContext.shortest_path_exn lc sorig1 sorig2)
      | (OEAStar r1, OEAStar r2) ->
        AtomLensIterate (gen_dnf_lens_zipper_internal lc r1 r2)
      | _ -> failwith "invalid"
    end

  and gen_clause_zipper (lc:LensContext.t)
      ((atoms_partitions1,strs1,_):ordered_exampled_clause)
      ((atoms_partitions2,strs2,_):ordered_exampled_clause)
    : clause_lens =
    let zipped_equivs = List.zip_exn atoms_partitions1 atoms_partitions2 in
    let atom_lens_perm_part_list_list =
      List.map
        ~f:(fun (a_list1,a_list2) ->
            let thingy = List.zip_exn a_list1 a_list2 in
            List.map
              ~f:(fun ((a1,i1),(a2,i2)) ->
                  (gen_atom_zipper lc a1 a2,(i1,i2)))
              thingy
          )
        zipped_equivs in
    let atom_lens_perm_part_list = List.concat atom_lens_perm_part_list_list in
    let atom_lens_perm_part_list_by_left_atom =
      List.sort
        ~cmp:(fun (_,(x,_)) (_,(y,_)) -> compare x y)
        atom_lens_perm_part_list in
    let (atom_lenses,perm_parts) = List.unzip
        atom_lens_perm_part_list_by_left_atom in
    (atom_lenses,Permutation.create_from_doubles_unsafe perm_parts,strs1,strs2)


  and gen_dnf_lens_zipper_internal
      (lc:LensContext.t)
      (r1:ordered_exampled_dnf_regex)
      (r2:ordered_exampled_dnf_regex)
    : dnf_lens =
    let zipped_equivs = List.zip_exn r1 r2 in
    let clause_lens_perm_part_list_list =
      List.map
        ~f:(fun (cl_list1,cl_list2) ->
            let thingy = List.zip_exn cl_list1 cl_list2 in
            List.map
              ~f:(fun ((cl1,i1),(cl2,i2)) ->
                  (gen_clause_zipper lc cl1 cl2,(i1,i2)))
              thingy
          )
        zipped_equivs in
    let clause_lens_perm_part_list = List.concat clause_lens_perm_part_list_list in
    let clause_lens_perm_part_list_by_left_clause =
      List.sort
        ~cmp:(fun (_,(x,_)) (_,(y,_)) -> compare x y)
        clause_lens_perm_part_list in
    let (clause_lenses,perm_parts) = List.unzip
        clause_lens_perm_part_list_by_left_clause in
    (clause_lenses,Permutation.create_from_doubles_unsafe perm_parts)

  let gen_dnf_lens_and_info_zipper
      (rc:RegexContext.t)
      (lc:LensContext.t)
      (r1:regex)
      (r2:regex)
      (exs:examples)
    : synthesis_info option =
    let count = ref 0 in
    let (lexs,rexs) = List.unzip exs in
    let rec gen_dnf_lens_zipper_queueing
        (queue:PQ.queue)
      : synthesis_info option =
      begin match PQ.pop queue with
        | None -> None
        | Some ((r1,r2,distance,expansions_performed,expansions_inferred,expansions_forced),_,q) ->
          incr(count);
          if !verbose then
            (print_endline "popped";
             print_endline ("r1: " ^ Pp.boom_pp_regex r1);
             print_endline "\n\n";
             print_endline ("r2: " ^ Pp.boom_pp_regex r2);
             print_endline "\n\n";
             print_endline ("distance: " ^ (string_of_int distance));
             print_endline "\n\n";
             print_endline ("count: " ^ (string_of_int !count));
             print_endline "\n\n";
             print_endline ("exps_perfed: " ^ (string_of_int expansions_performed));
             print_endline "\n\n";
             print_endline ("exps_inferred: " ^ (string_of_int expansions_inferred));
             print_endline "\n\n";
             print_endline ("exps_forced: " ^ (string_of_int expansions_forced));
             print_endline ("\n\n\n"));
          if !infer_expansions then
            let (r1,r2,exs) =
              expand_full_outermost_required_expansions
                rc
                lc
                r1
                r2
            in
            let expansions_performed = expansions_performed+exs in
            let expansions_inferred = expansions_inferred+exs in
            let expansions_forced = expansions_forced+exs in
            let s1 = get_full_current_level_user_defined_rep_set lc r1 in
            let s2 = get_full_current_level_user_defined_rep_set lc r2 in
            let problem_elements =
              (List.map
                 ~f:(fun e -> Left e)
                 (StringIntSet.as_list (StringIntSet.minus s1 s2)))
              @
              (List.map
                 ~f:(fun e -> Right e)
                 (StringIntSet.as_list (StringIntSet.minus s2 s1)))
            in
            if List.is_empty problem_elements then
              (
                let exampled_r1_opt = regex_to_exampled_dnf_regex rc lc r1 lexs in
                let exampled_r2_opt = regex_to_exampled_dnf_regex rc lc r2 rexs in
                if distance = 0 || (not !short_circuit) then
                  begin match (exampled_r1_opt,exampled_r2_opt) with
                    | (Some exampled_r1,Some exampled_r2) ->
                      let e_o_r1 = to_ordered_exampled_dnf_regex exampled_r1 in
                      let e_o_r2 = to_ordered_exampled_dnf_regex exampled_r2 in
                      begin match compare_ordered_exampled_dnf_regexs e_o_r1 e_o_r2 with
                        | EQ ->
                          Some (
                            {
                              l = gen_dnf_lens_zipper_internal lc e_o_r1 e_o_r2;
                              specs_visited = !count;
                              expansions_performed = expansions_performed;
                              expansions_inferred = expansions_inferred;
                              expansions_forced = expansions_forced;
                            })
                            
                        | _ ->
                          let rx_list =
                            expand_once
                              rc
                              r1
                              r2
                          in

                          let queue_elements =
                            List.map
                              ~f:(fun (r1,r2) ->
                                  let distance = retrieve_distance lc r1 r2 in
                                  (r1,r2,distance,expansions_performed+1,expansions_inferred,expansions_forced))
                              rx_list
                          in

                          gen_dnf_lens_zipper_queueing
                            (PQ.push_all
                               q
                               queue_elements)
                      end
                    | _ -> None
                  end
                else
                  let rx_list =
                    expand_once
                      rc
                      r1
                      r2
                  in

                  let queue_elements =
                    List.map
                      ~f:(fun (r1,r2) ->
                          let distance = retrieve_distance lc r1 r2 in
                          (r1,r2,distance,expansions_performed+1,expansions_inferred,expansions_forced))
                      rx_list
                  in

                  gen_dnf_lens_zipper_queueing
                    (PQ.push_all
                       q
                       queue_elements))
            else
              let new_problems =
                List.concat_map
                  ~f:(fun se ->
                      begin match se with
                        | Left (v,star_depth) ->
                          let exposes = expose_full_userdef rc lc v star_depth r2 in
                          if List.is_empty exposes then
                            let (r1_expanded,expcount) =
                              force_expand_userdef
                                rc
                                lc
                                v
                                r1
                            in
                            [(r1_expanded,r2,expcount)]
                          else
                            List.map ~f:(fun (e,exp) -> (r1,e,exs+exp)) exposes
                        | Right (v,star_depth) ->
                          let exposes = expose_full_userdef rc lc v star_depth r1 in
                          if List.is_empty exposes then
                            failwith ("shoulda handled earlier  " ^ v ^ "   " ^ (string_of_int star_depth) ^ "\n\n" ^ (Pp.boom_pp_regex r1)
                                      ^ "\n\n" ^ (Pp.boom_pp_regex r2))
                            (*let (r2_expanded,expcount) =
                              force_expand_userdef
                                r
                              c
                                lc
                                v
                                r2
                              in
                              [(r1,r2_expanded,expcount+exs)]*)
                          else
                            List.map ~f:(fun (e,exp) -> (e,r2,exs+exp)) exposes
                      end)
                  problem_elements
              in
              let queue_elements =
                List.map
                  ~f:(fun (r1,r2,exp) ->
                      let distance = retrieve_distance lc r1 r2 in
                      (r1,r2,distance,expansions_performed+exp,expansions_inferred+exp,expansions_forced))
                  new_problems
              in
              gen_dnf_lens_zipper_queueing
                (PQ.push_all
                   q
                   queue_elements)
          else
            let exampled_r1_opt = regex_to_exampled_dnf_regex rc lc r1 lexs in
            let exampled_r2_opt = regex_to_exampled_dnf_regex rc lc r2 rexs in
            if distance = 0 || (not !short_circuit) then
              begin match (exampled_r1_opt,exampled_r2_opt) with
                | (Some exampled_r1,Some exampled_r2) ->
                  let e_o_r1 = to_ordered_exampled_dnf_regex exampled_r1 in
                  let e_o_r2 = to_ordered_exampled_dnf_regex exampled_r2 in
                  begin match compare_ordered_exampled_dnf_regexs e_o_r1 e_o_r2 with
                    | EQ ->
                          Some (
                            {
                              l = gen_dnf_lens_zipper_internal lc e_o_r1 e_o_r2;
                              specs_visited = !count;
                              expansions_performed = expansions_performed;
                              expansions_inferred = expansions_inferred;
                              expansions_forced = expansions_forced;
                            })
                    | _ ->
                      let rx_list =
                        expand_once
                          rc
                          r1
                          r2
                      in

                      let queue_elements =
                        List.map
                          ~f:(fun (r1,r2) ->
                              let distance = retrieve_distance lc r1 r2 in
                              (r1,r2,distance,expansions_performed+1,expansions_inferred,expansions_forced))
                          rx_list
                      in

                      gen_dnf_lens_zipper_queueing
                        (PQ.push_all
                           q
                           queue_elements)
                  end
                | _ -> None
              end
            else
              let rx_list =
                expand_once
                  rc
                  r1
                  r2
              in

              let queue_elements =
                List.map
                  ~f:(fun (r1,r2) ->
                      let distance = retrieve_distance lc r1 r2 in
                      (r1,r2,distance,expansions_performed+1,expansions_inferred,expansions_forced))
                  rx_list
              in

              gen_dnf_lens_zipper_queueing
                (PQ.push_all
                   q
                   queue_elements)
      end
    in
    gen_dnf_lens_zipper_queueing
      (PQ.from_list
         [((r1,r2,0,0,0,0))])

  
  let gen_dnf_lens (rc:RegexContext.t) (lc:LensContext.t) (r1:regex) (r2:regex)
      (exs:examples)
    : dnf_lens option =
    Option.map ~f:(fun x -> x.l) (gen_dnf_lens_and_info_zipper rc lc r1 r2 exs)

  let expansions_performed_for_gen
      (rc:RegexContext.t)
      (lc:LensContext.t)
      (r1:regex)
      (r2:regex)
      (exs:examples)
    : int option =
    Option.map ~f:(fun x -> x.expansions_performed) (gen_dnf_lens_and_info_zipper rc lc r1 r2 exs)

  let specs_visited_for_gen
      (rc:RegexContext.t)
      (lc:LensContext.t)
      (r1:regex)
      (r2:regex)
      (exs:examples)
    : int option =
    Option.map ~f:(fun x -> x.specs_visited) (gen_dnf_lens_and_info_zipper rc lc r1 r2 exs)

  let expansions_inferred_for_gen
      (rc:RegexContext.t)
      (lc:LensContext.t)
      (r1:regex)
      (r2:regex)
      (exs:examples)
    : int option =
    Option.map ~f:(fun x -> x.expansions_inferred) (gen_dnf_lens_and_info_zipper rc lc r1 r2 exs)

  let expansions_forced_for_gen
      (rc:RegexContext.t)
      (lc:LensContext.t)
      (r1:regex)
      (r2:regex)
      (exs:examples)
    : int option =
    Option.map ~f:(fun x -> x.expansions_forced) (gen_dnf_lens_and_info_zipper rc lc r1 r2 exs)

  let gen_lens
      (rc:RegexContext.t)
      (lc:LensContext.t)
      (r1:regex)
      (r2:regex)
      (exs:examples)
    : lens option =
      let dnf_lens_option = gen_dnf_lens rc lc r1 r2 exs in
      Option.map
        ~f:dnf_lens_to_lens
        dnf_lens_option
end

module EXPANDCOUNT_SYNTHESIZER =
  DNFSynth(EXPANDCOUNT_PQUEUE)

let expansions_performed_for_gen = EXPANDCOUNT_SYNTHESIZER.expansions_performed_for_gen
let specs_visited_for_gen = EXPANDCOUNT_SYNTHESIZER.specs_visited_for_gen
let expansions_inferred_for_gen = EXPANDCOUNT_SYNTHESIZER.expansions_inferred_for_gen
let expansions_forced_for_gen = EXPANDCOUNT_SYNTHESIZER.expansions_forced_for_gen

let gen_lens
    (rc:RegexContext.t)
    (lc:LensContext.t)
    (r1:regex)
    (r2:regex)
    (exs:examples)
  : lens option =
  if !verbose then
    print_endline "Synthesis Start";
  let rc_orig = rc in
  let (r1,r2,rc) =
    if !use_iterative_deepen_strategy then
      let (r1,c1) = iteratively_deepen r1 in
      let (r2,c2) = iteratively_deepen r2 in
      let rc = 
        RegexContext.merge_contexts_exn
          rc
          (RegexContext.merge_contexts_exn c1 c2)
      in
      (r1,r2,rc)
    else
      (r1,r2,rc)
  in
  let lens_option =
    (if !naive_strategy then
      Some (gen_lens_naive rc lc r1 r2 exs)
     else if !naive_pqueue then
       EXPANDCOUNT_SYNTHESIZER.gen_lens rc lc r1 r2 exs
     else
       EXPANDCOUNT_SYNTHESIZER.gen_lens rc lc r1 r2 exs)
  in
  if !verbose then
    print_endline "Synthesis End";
  Option.map
    ~f:(simplify_lens % (make_lens_safe_in_smaller_context rc_orig rc))
    lens_option
  
