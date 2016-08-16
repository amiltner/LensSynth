open Core.Std
open Lenscontext
open Converter
open Regexcontext
open Lang
open Lens_utilities
open Util
open Permutation
open Transform
open Priority_queue
open Normalized_lang
open Language_equivalences
open Consts

(*let rec map_and_abstract (mc:mapsbetweencontext)
                         (r:ordered_exampled_dnf_regex)
                         (s:ordered_exampled_dnf_regex)
                         : dnf_regex * dnf_regex * mapsbetweencontext * bool=
  let rec map_and_abstract_internal (mc:mapsbetweencontext)
                                    (r:ordered_exampled_dnf_regex)
                                    (s:ordered_exampled_dnf_regex)
                                    (altered:bool)
                                    : ordered_exampled_dnf_regex
                                      * ordered_exampled_dnf_regex
                                      * mapsbetweencontext
                                      * bool =
    begin match (r,s) with
    | (csh1::cst1,csh2::cst2) ->
        let (head_element_l,_) = List.hd_exn csh1 in
        let (head_element_r,_) = List.hd_exn csh2 in
        begin match compare_ordered_exampled_clauses
                    head_element_l
                    head_element_r with
        | GT -> let (first,second,mc,altered) =
                  map_and_abstract_internal mc cst1 s altered in
                (csh1::first,second,mc,altered)
        | LT -> let (first,second,mc,altered) =
                  map_and_abstract_internal mc r cst2 altered in
                (first,csh2::second,mc,altered)
        | EQ ->
            let (first,second,mc,altered) =
              map_and_abstract_internal mc cst1 cst2 altered in
            if (is_immutable_clause head_element_l) then
              (csh1::first,csh2::second,mc,altered)
            else
              let zipped_data = zip_nondist csh1 csh2 in
              let (xrev,yrev, mc) = List.fold_left
              ~f:(fun (xs,ys,mc) (xo,yo) ->
                begin match (xo,yo) with
                | (None, Some y) -> (xs,y::ys,mc)
                | (Some x, None) -> (x::xs,ys,mc)
                | (None, None) -> failwith "NEVER SHOULD GET HERE"
                | (Some (x,xi), Some (y,yi)) ->
                    let rx_x = ordered_exampled_clause_to_clause x in
                    let rx_y = ordered_exampled_clause_to_clause y in
                    let (mc,n) = add_to_context mc [rx_x] [rx_y] in
                    let added_clause = ([[(OEAMappedUserDefined n,0)]],["";""],[]) in
                    ((added_clause,xi)::xs,(added_clause,yi)::ys,mc)
                end
              )
              ~init:([],[],mc)
              zipped_data
              in
              ((List.rev xrev)::first,(List.rev yrev)::second,mc,true)
        end
    | _ -> (r,s,mc,altered)
    end
  in
  (* TODO: remove clauses when there's only one unset *)
  let (r',s',mc,got_something) = map_and_abstract_internal mc r s false in
  (ordered_exampled_dnf_regex_to_dnf_regex r'
  ,ordered_exampled_dnf_regex_to_dnf_regex s'
  ,mc
  ,got_something)*)

let num_stars_current_level_clause ((atoms,_):clause) : int =
  let num_stars_current_level_atom (a:atom) : int =
    begin match a with
    | AUserDefined _ -> 0
    | AStar _ -> 1
    end in
  List.fold_left
    ~f:(fun acc a -> acc + (num_stars_current_level_atom a))
    ~init:0
    atoms

let num_stars_current_level_regex (r:dnf_regex) : int =
  List.fold_left
    ~f:(fun acc cl -> acc + (num_stars_current_level_clause cl))
    ~init:0
    r

let empty_or_not_star_expansion_dnf (r:dnf_regex) : dnf_regex =
  or_dnf_regexs
    empty_dnf_string
    (concat_dnf_regexs
      (List.rev r)
      (singleton_atom (AStar r)))

let quotient_product_expansion_dnf (n:int) (r:dnf_regex) : dnf_regex =
  concat_dnf_regexs
    (List.rev (quotiented_star_dnf r n))
    (singleton_atom (AStar (exponentiate_dnf r n)))

let expand_atom_rewrite (expansion_function:dnf_regex -> dnf_regex)
                        (r:dnf_regex)
                        (expanded_atom:int) : dnf_regex =
  let expand_clause ((atoms,strings):clause) (num_atom:int) : clause list =
    let (expanded_clause,_) = List.fold_left
      ~f:(fun (acc,atoms_passed) i ->
        begin match acc with
        | None -> let atom = List.nth_exn atoms i in
          begin match atom with
          | AUserDefined _ -> (None,atoms_passed)
          | AStar r' ->
              if atoms_passed = num_atom then
                let expanded_atom = expansion_function r' in
                let left_subclause = (List.take atoms i,List.take strings (i+1))
                in
                let right_subclause = (List.drop atoms (i+1), List.drop strings
                (i+1)) in
                let result = concat_clause_dnf_rx
                  left_subclause
                  (concat_dnf_rx_clause expanded_atom right_subclause) in
                (Some result, atoms_passed+1)
              else
                (None,atoms_passed+1)
          end
        | Some _ -> (acc,atoms_passed)
        end)
      ~init:(None,0)
      (range 0 ((List.length atoms)-1)) in
    begin match expanded_clause with
    | None -> failwith "incorrect algo"
    | Some expanded_clause -> expanded_clause
    end in

  let (r',_) = List.fold_left
    ~f:(fun (acc,atoms_passed) cl ->
        let num_atoms_here = num_stars_current_level_clause cl in
        if atoms_passed <= expanded_atom
            && (expanded_atom < atoms_passed + num_atoms_here) then
          (acc @ (expand_clause cl (expanded_atom-atoms_passed)),atoms_passed+num_atoms_here)
        else
          (acc @ [cl],atoms_passed+num_atoms_here))
    ~init:([],0)
    r in
  r'

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

let gen_dnf_lens_zipper (rc:RegexContext.t)
    (lc:LensContext.t)
                        (r1:regex)
                        (r2:regex)
                        (exs:examples)
                      : dnf_lens option =
  let (lexs,rexs) = List.unzip exs in
  let max_size = max (true_max_size rc r1) (true_max_size rc r2) in
  let rec gen_dnf_lens_zipper_queueing
        (queue:queue_element Priority_Queue.t)
        : (dnf_lens * regex * regex) option =
    begin match Priority_Queue.pop queue with
    | None -> None
    | Some (queue_element,_,q) ->
        begin match queue_element with
        | QERegexCombo (r1,r2,star_expansions) ->
          begin match expand_required_expansions rc lc r1 r2 with
          | Some (r1',r2') ->
(*let (r1',r2') = (r1,r2) in*)
              (*print_endline ("\n\n\npopped " ^ (Float.to_string p));
              print_endline (Pp.pp_regexp (r1'));
              print_endline (Pp.pp_regexp (r2'));
                print_endline (string_of_int (List.length (fst mc)));*)
let exampled_r1_opt = regex_to_exampled_dnf_regex rc lc r1' lexs in
let exampled_r2_opt = regex_to_exampled_dnf_regex rc lc r2' rexs in
              begin match (exampled_r1_opt,exampled_r2_opt) with
              | (Some exampled_r1,Some exampled_r2) ->
                  let e_o_r1 = to_ordered_exampled_dnf_regex exampled_r1 in
                  let e_o_r2 = to_ordered_exampled_dnf_regex exampled_r2 in
                  (*print_endline "\n\n\n";
                  print_endline (Pp.pp_exampled_dnf_regex exampled_r1);
                  print_endline "\n";
                  print_endline (Pp.pp_exampled_dnf_regex exampled_r2);*)
                  begin match compare_ordered_exampled_dnf_regexs e_o_r1 e_o_r2 with
                  | EQ -> 
                      Some ((gen_dnf_lens_zipper_internal lc e_o_r1 e_o_r2),
                           r1',
                           r2' )
                  | _ ->
                      (*let (dr1',dr2',mc',got_something) = map_and_abstract
                        mc
                        e_o_r1
                        e_o_r2 in

                      let (r1',r2') = if got_something then
                          (smart_dnf_regex_to_regex dr1',smart_dnf_regex_to_regex dr2')
                      else
                        (r1',r2')
                        in*)

                      let rx_list =
                        retrieve_transformation_queue_elements
                          max_size
                          rc
                          lc
                          r1'
                          r2'
                          star_expansions
                      in
                      gen_dnf_lens_zipper_queueing
                        (Priority_Queue.push_all q
                          rx_list)
                  end
              | _ -> None
              end
          | None -> gen_dnf_lens_zipper_queueing q
          end
        | QEGenerator gen ->
            gen_dnf_lens_zipper_queueing
              (Priority_Queue.push_all q (gen ()))
        end
    end
  in
  let dlrro =
    gen_dnf_lens_zipper_queueing
      (Priority_Queue.create_from_list
         [(QERegexCombo(r1,r2,0),1.0)])
  in
  let dlo = Option.map ~f:(fun (dl,_,_) -> dl) dlrro in
  dlo

  (*List.fold_left
  ~f:(fun acc n ->
    begin match acc with
   | None ->
        let combos = apply_transformations c r1 r2 n in
        List.fold_left
          ~f:(fun acc' (r1',r2') ->
            begin match acc' with
            | None -> 
                let exampled_r1_opt = regex_to_exampled_dnf_regex e_c r1' lexs in
                let exampled_r2_opt = regex_to_exampled_dnf_regex e_c r2' rexs in
                begin match (exampled_r1_opt,exampled_r2_opt) with
                | (Some exampled_r1,Some exampled_r2) ->
                    let e_o_r1 = to_ordered_exampled_dnf_regex exampled_r1 in
                    let e_o_r2 = to_ordered_exampled_dnf_regex exampled_r2 in
                    begin match compare_ordered_exampled_dnf_regexs e_o_r1 e_o_r2 with
                    | EQ -> 
                        Some (gen_dnf_lens_zipper_internal e_o_r1 e_o_r2)
                    | _ -> None
                    end
                | _ -> None
                end
            | _ -> acc'
            end)
          ~init:None
          combos
    | _ -> acc
    end)
  ~init:None
  (range 0 7)*)


let gen_dnf_lens (rc:RegexContext.t) (lc:LensContext.t) (r1:regex) (r2:regex)
(exs:examples)
  : dnf_lens option =
    gen_dnf_lens_zipper rc lc r1 r2 exs

let gen_lens (rc:RegexContext.t) (lc:LensContext.t) (r1:regex) (r2:regex)
             (exs:examples) : lens option =
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
  let dnf_lens_option = gen_dnf_lens rc lc r1 r2 exs in
  Option.map
    ~f:(simplify_lens
        % (make_lens_safe_in_smaller_context rc_orig rc)
        % dnf_lens_to_lens)
    dnf_lens_option
    
