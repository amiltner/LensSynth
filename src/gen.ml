open Core.Std
open Fasteval
open Util
open Lang
open Lens
open Eval
open Util
open Pp
open Permutation
open Transform
open Priority_queue

let rec all_match (c:context) (r:regex) (ss:string list) : bool =
  List.fold_left
  ~f:(fun acc s -> acc && eval_regex c r s)
  ~init:true
  ss

let rec gen_lenses (c:context) (s:regex) (t:regex) (exs:examples) : lens list =
  let structural_lenses = (begin match (s,t) with
  | (RegExBase s1, RegExBase s2) ->
      let (exss,exst) = List.unzip exs in
      if ((all_match c s exss) && (all_match c t exst)) then
        [ConstLens (s1, s2)]
      else
        []
  | (RegExConcat (s1,s2), RegExConcat (t1,t2)) ->
      let gen_concat permutation1 permutation2 generator = (
        let (sexs,texs) = List.unzip exs in
        let sexs_splits = List.map
          ~f:(retrieve_regex_concat_split c s1 s2)
          sexs in
        let texs_splits = List.map
          ~f:(retrieve_regex_concat_split c t1 t2)
          texs in
        begin match (distribute_option sexs_splits, distribute_option texs_splits) with
        | (Some sexsplits, Some texsplits) ->
          let (t1,t2) = permutation1 (t1,t2) in
          let (sexs1,sexs2) = List.unzip sexsplits in
          let (texs1,texs2) = permutation2 (List.unzip texsplits) in
          let lenses1 = gen_lenses c s1 t1 (List.zip_exn sexs1 texs1) in
          let lenses2 = gen_lenses c s2 t2 (List.zip_exn sexs2 texs2) in
          cartesian_map
            generator
            lenses1
            lenses2
        | _ -> []
        end
      ) in
      gen_concat
        (fun (x,y) -> (x,y))
        (fun (x,y) -> (x,y))
        (fun l1 l2 -> ConcatLens (l1,l2)) @
      gen_concat
        (fun (x,y) -> (y,x))
        (fun (x,y) -> (y,x))
        (fun l1 l2 -> SwapLens (l1,l2))
  | (RegExOr (s1,s2), RegExOr (t1,t2)) ->
      let (sexs,texs) = List.unzip exs in
      let sexs_choices = List.map
        ~f:(retrieve_regex_or_choice c s1 s2)
        sexs in
      let texs_choices = List.map
        ~f:(retrieve_regex_or_choice c t1 t2)
        texs in
      begin match (distribute_option sexs_choices, distribute_option texs_choices) with
      | (Some sexchoices, Some texchoices) ->
          let split_examples = List.fold_left
          ~f:(fun (accs) ((sexchoice,texchoice),ex) ->
            begin match accs with
            | None -> None
            | Some (leftacc,rightacc) ->
                begin match (sexchoice,texchoice) with
                | (false,false) -> Some (ex::leftacc,rightacc)
                | (true,true) -> Some (leftacc,ex::rightacc)
                | _ -> None
                end
            end)
          ~init:(Some ([],[]))
          (List.zip_exn (List.zip_exn sexchoices texchoices) exs) in
          begin match split_examples with
          | None -> []
          | Some (exs1,exs2) ->
              let ls1 = gen_lenses c s1 t1 exs1 in
              let ls2 = gen_lenses c s2 t2 exs2 in
              cartesian_map
                (fun x y -> UnionLens (x,y))
                ls1
                ls2
          end
      | _ -> []
      end
    
  | (RegExStar s', RegExStar t') ->
      let (sexs,texs) = List.unzip exs in
      let sexs_splits = List.map
        ~f:(retrieve_regex_star_splits c s')
        sexs in
      let texs_splits = List.map
        ~f:(retrieve_regex_star_splits c t')
        texs in
      begin match (distribute_option sexs_splits, distribute_option texs_splits) with
      | (Some sexs_splits,Some texs_splits) ->
          let split_examples = List.fold_left
          ~f:(fun (accs) ((sexsplit,texsplit)) ->
            begin match accs with
            | None -> None
            | Some (acc) ->
                (begin match (List.zip sexsplit texsplit) with
                | None -> None
                | Some zip -> Some (zip @ acc)
                end)
            end)
          ~init:(Some [])
          (List.zip_exn sexs_splits texs_splits) in

          begin match split_examples with
          | None -> []
          | Some l ->
              let ls' = gen_lenses c s' t' l in
              List.map
              ~f:(fun x -> IterateLens x)
              ls'
          end
      | _ -> []
      end
  | (RegExUserDefined typ1, RegExUserDefined typ2) -> []
  | _ -> []
  end) in
  let (sexs,texs) = List.unzip exs in
  if (s = t
      && sexs = texs
      && all_match c s sexs) then
        IdentityLens :: structural_lenses
      else
        structural_lenses

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


let modular_n_expand_atom (r:dnf_regex) (expanded_atom:int) : dnf_regex =
  []

let rec gen_atom_lens (c:context) (a1:atom) (a2:atom)
                      (exs:examples) (expand_count:int) : atom_lens option =
  begin match (a1,a2) with
  | (AStar r1, AStar r2) ->
      let (sexs,texs) = List.unzip exs in
      let sexs_splits = List.map
        ~f:(retrieve_dnf_star_splits c r1)
        sexs in
      let texs_splits = List.map
        ~f:(retrieve_dnf_star_splits c r2)
        texs in
      begin match (distribute_option sexs_splits, distribute_option texs_splits) with
      | (Some sexs_splits,Some texs_splits) ->
          let split_examples = List.fold_left
          ~f:(fun (accs) ((sexsplit,texsplit)) ->
            begin match accs with
            | None -> None
            | Some (acc) ->
                (begin match (List.zip sexsplit texsplit) with
                | None -> None
                | Some zip -> Some (zip @ acc)
                end)
            end)
          ~init:(Some [])
          (List.zip_exn sexs_splits texs_splits) in

          begin match split_examples with
          | None ->  None
          | Some l ->
              begin match gen_dnf_lens_internal c r1 r2 l expand_count with
                      | None -> None
                      | Some l -> Some (AIterate l)
                      end
          end
      | _ -> None
      end








  | (AUserDefined r1, AUserDefined r2) ->
      if r1 = r2 && (List.for_all ~f:(fun (le,re) -> le = re) exs) then
        Some AIdentity
      else
        None
  | _ -> None
  end

and gen_clause_lens (c:context) ((atoms1,strings1):clause)
                    ((atoms2,strings2):clause)
                    (exs:examples) (expand_count:int) : clause_lens option =
  let len = List.length atoms1 in
  if len <> List.length atoms2 then
    None
  else if len = 0 && (List.for_all ~f:((=) (List.hd_exn strings1, List.hd_exn
  strings2)) exs) then
    Some ([],Permutation.create [], strings1, strings2)
  else
    let (lexs,rexs) = List.unzip exs in
    let lexs_splits_option = List.map ~f:(retrieve_atom_splits c
            (atoms1,strings1)) lexs in
    let rexs_splits_option = List.map ~f:(retrieve_atom_splits c
            (atoms2,strings2)) rexs in
    begin match (distribute_option lexs_splits_option,distribute_option rexs_splits_option) with
    | (Some lexs_splits,Some rexs_splits) ->
        let left_atom_examples = transpose_safe_empty_exn len lexs_splits in
        let right_atom_examples = transpose_safe_empty_exn len rexs_splits in
        fold_until_completion
          (fun (invalid_parts,valid_parts) ->
            let perm_guess_option = Permutation.create_from_constraints
                          len
                          invalid_parts
                          valid_parts in
            begin match perm_guess_option with
            | None -> Right None
            | Some (perm, guesses) ->
              let permuted_atoms2 = Permutation.apply_inverse_to_list_exn perm atoms2 in
              let zipped_atoms = List.zip_exn atoms1 permuted_atoms2 in
              let atom_examples = List.zip_exn left_atom_examples
                (Permutation.apply_inverse_to_list_exn perm right_atom_examples) in
              let atom_examples_list = List.zip_exn zipped_atoms atom_examples in
              let atom_lens_options = List.map
                ~f:(fun ((c1,c2),(lexs,rexs)) -> gen_atom_lens c c1 c2
                (List.zip_exn lexs rexs) expand_count)
                atom_examples_list in
              begin match distribute_option atom_lens_options with
              | None -> let (goods,bads) = List.partition_tf
                          ~f:(fun (i,j) -> (List.nth atom_lens_options i) = None)
                          guesses in
                        Left (bads@invalid_parts,goods@valid_parts)
              | Some ls -> Right (Some (ls,perm,strings1,strings2))
              end
            end)
            ([],[])
    | (_,_) -> None
    end

and gen_dnf_lens_expand_beneath (c:context) (clauses1:dnf_regex) (clauses2:dnf_regex)
                 (exs:examples) (expand_count:int) : dnf_lens option =
  let len = List.length clauses1 in
  if len <> List.length clauses2 then
    None
  else if len = 0 && exs = [] then
    Some ([],Permutation.create [])
  else
    let (lexs,rexs) = List.unzip exs in
    let lexs_choices_option = distribute_option (List.map
        ~f:(retrieve_dnf_clause_choices c clauses1)
        lexs) in
    let rexs_choices_option = distribute_option (List.map
        ~f:(retrieve_dnf_clause_choices c clauses2)
        rexs) in
    begin match (lexs_choices_option, rexs_choices_option) with
    | (Some lexs_choices, Some rexs_choices) -> 
        let choices = List.zip_exn lexs_choices rexs_choices in
        let dedup_choices = List.dedup choices in
        fold_until_completion
          (fun (invalid_parts,required_parts) ->
            let perm_guess_option = Permutation.create_from_constraints
                          len
                          invalid_parts
                          required_parts in
            begin match perm_guess_option with
            | None -> Right None
            | Some (perm, guesses) ->
                    let (lchoices,rchoices) = List.unzip choices in
                    let lchoice_lexample_pairs = List.zip_exn lexs lchoices in
                    let rchoice_rexample_pairs = List.zip_exn rexs rchoices in
                    let lclauseexs = bucketize_pairs len lchoice_lexample_pairs in
                    let rclauseexs = Permutation.apply_inverse_to_list_exn perm
                      (bucketize_pairs len rchoice_rexample_pairs) in
                    let clause_exs = List.zip_exn lclauseexs rclauseexs in
                    let permuted_clauses2 = Permutation.apply_inverse_to_list_exn perm clauses2 in
                    let zipped_clauses = List.zip_exn clauses1 permuted_clauses2 in
                    let clause_exs_pairs = List.zip_exn zipped_clauses clause_exs in
                    let clause_lens_options = List.mapi
                      ~f:(fun i ((c1,c2),(lexs,rexs)) ->
                        gen_clause_lens c c1 c2
                        (List.zip_exn lexs rexs) expand_count)
                      clause_exs_pairs in
                    begin match distribute_option clause_lens_options with
                    | None -> let (goods,bads) = List.partition_tf
                          ~f:(fun (i,j) -> (List.nth clause_lens_options i) = None)
                          guesses in
                        if (List.length bads) = 0 then
                          Right None
                        else
                          Left (bads@invalid_parts,goods@required_parts)

                    | Some ls -> Right (Some (ls,perm))
                    end
            end
          )
          ([],dedup_choices)
    | (_,_) -> None
    end

and gen_dnf_lens_expand_here (c:context) (clauses1:dnf_regex) (clauses2:dnf_regex)
                        (exs:examples) (expand_count:int) : dnf_lens option =
  if expand_count = 0 then
    None
  else
    let num_stars_left = num_stars_current_level_regex clauses1 in
    let num_stars_right = num_stars_current_level_regex clauses2 in
    let num_stars_current_level = num_stars_left + num_stars_right in
    let split_locations = range 0 (num_stars_current_level - 1) in
    let equivalence_size = max (List.length clauses1) (List.length clauses2) in
    let primes_beneath = primes_beneath_n equivalence_size in
    let rewrites = empty_or_not_star_expansion_dnf::
      List.map
        ~f:quotient_product_expansion_dnf
        primes_beneath in
    List.fold_left
      ~f:(fun acc rewrite ->
        begin match acc with
        | Some lens -> acc
        | None ->
          List.fold_left
            ~f:(fun acc loc ->
              begin match acc with
              | Some _ -> acc
              | None -> let (clauses1',clauses2') =
                (if loc < num_stars_left then
                    (expand_atom_rewrite rewrite clauses1 loc, clauses2)
                  else
                    (clauses1, expand_atom_rewrite rewrite clauses2 (loc-num_stars_left))) in
                gen_dnf_lens_internal c clauses1' clauses2' exs (expand_count-1)
              end)
            ~init:None
            split_locations
        end)
      ~init:None
      rewrites

and gen_atom_zipper (atom1:ordered_exampled_atom)
                    (atom2:ordered_exampled_atom)
                    : atom_lens =
  begin match (atom1,atom2) with
  | (OEAUserDefined _,OEAUserDefined _) -> AIdentity
  | (OEAStar r1, OEAStar r2) ->
      AIterate (gen_dnf_lens_zipper_internal r1 r2)
  | _ -> failwith "invalid"
  end

and gen_clause_zipper ((atoms_partitions1,strs1,_):ordered_exampled_clause)
                      ((atoms_partitions2,strs2,_):ordered_exampled_clause)
                      : clause_lens =
  let zipped_equivs = List.zip_exn atoms_partitions1 atoms_partitions2 in
  let atom_lens_perm_part_list_list =
    List.map
      ~f:(fun (a_list1,a_list2) ->
        let thingy = List.zip_exn a_list1 a_list2 in
        List.map
          ~f:(fun ((a1,i1),(a2,i2)) ->
            (gen_atom_zipper a1 a2,(i1,i2)))
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


and gen_dnf_lens_zipper_internal (r1:ordered_exampled_dnf_regex)
                                 (r2:ordered_exampled_dnf_regex)
                               : dnf_lens =
  let zipped_equivs = List.zip_exn r1 r2 in
  let clause_lens_perm_part_list_list =
    List.map
      ~f:(fun (cl_list1,cl_list2) ->
        let thingy = List.zip_exn cl_list1 cl_list2 in
        List.map
          ~f:(fun ((cl1,i1),(cl2,i2)) ->
            (gen_clause_zipper cl1 cl2,(i1,i2)))
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

and gen_dnf_lens_internal (c:context) (clauses1:dnf_regex) (clauses2:dnf_regex)
                 (exs:examples) (expand_count:int) : dnf_lens option =
  let expanded_here = gen_dnf_lens_expand_here c clauses1 clauses2 exs expand_count in
  if expanded_here <> None then
    expanded_here
  else
    gen_dnf_lens_expand_beneath c clauses1 clauses2 exs expand_count

let gen_dnf_lens_zipper (c:context)
                        (e_c:evaluation_context)
                        (r1:regex)
                        (r2:regex)
                        (exs:examples)
                      : dnf_lens option =
  let (lexs,rexs) = List.unzip exs in
  let max_size = max (true_max_size c r1) (true_max_size c r2) in
  let rec gen_dnf_lens_zipper_queueing
        (queue:(regex * regex * int) Priority_Queue.t)
        : dnf_lens option =
    begin match Priority_Queue.pop queue with
    | None -> None
    | Some ((r1,r2,star_expansions),p,q) ->
        if star_expansions <= 0 then
          gen_dnf_lens_zipper_queueing q
        else
          begin match expand_required_expansions c r1 r2 with
          | Some (r1',r2') ->
              (*print_endline ("\n\n\npopped " ^ (Float.to_string p));
              print_endline (Pp.pp_regexp r1');
              print_endline (Pp.pp_regexp r2');*)
          let exampled_r1_opt = regex_to_exampled_dnf_regex e_c r1' lexs in
          let exampled_r2_opt = regex_to_exampled_dnf_regex e_c r2' rexs in
          begin match (exampled_r1_opt,exampled_r2_opt) with
          | (Some exampled_r1,Some exampled_r2) ->
              (*print_endline "\n\n\n";
              print_endline (Pp.pp_exampled_dnf_regex exampled_r1);
              print_endline "\n";
              print_endline (Pp.pp_exampled_dnf_regex exampled_r2);*)
              let e_o_r1 = to_ordered_exampled_dnf_regex exampled_r1 in
              let e_o_r2 = to_ordered_exampled_dnf_regex exampled_r2 in
              begin match compare_ordered_exampled_dnf_regexs e_o_r1 e_o_r2 with
              | EQ -> 
                  Some (gen_dnf_lens_zipper_internal e_o_r1 e_o_r2)
              | _ ->
                  let rx_list = apply_transformations max_size c r1' r2' 1 in
                  gen_dnf_lens_zipper_queueing
                    (Priority_Queue.push_all q
                    (List.map
                      ~f:(fun (r1,r2) ->
                        (((r1,r2,star_expansions-1),
                        (2.0 ** (Float.of_int (10-star_expansions)))
                        *.
                        (retrieve_priority r1 r2))))
                      rx_list))
              end
          | _ -> None
          end
          | None -> gen_dnf_lens_zipper_queueing q
          end
    end
  in
  gen_dnf_lens_zipper_queueing (Priority_Queue.create_from_list
  [((r1,r2,10),1.0)])

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


let gen_dnf_lens (c:context) (e_c:evaluation_context) (r1:regex) (r2:regex)
                 (exs:examples) : dnf_lens option =
  gen_dnf_lens_zipper c e_c r1 r2 exs

let gen_lens (c:context) (e_c:evaluation_context) (r1:regex) (r2:regex)
             (exs:examples) : lens option =
  print_endline (Pp.pp_regexp r1);
  print_endline (Pp.pp_regexp r2);
  print_endline (String.concat ~sep:";" (List.map ~f:(fun (s1,s2) ->
    "("^s1^","^s2^")") exs));
  let dnf_lens_option = gen_dnf_lens_zipper c e_c r1 r2 exs in
  Option.map
    ~f:(Fn.compose simplify_lens dnf_lens_to_lens)
    dnf_lens_option
  (*let (lexs,rexs) = List.unzip exs in
  let exampled_r1_opt = regex_to_exampled_dnf_regex c r1 lexs in
  let exampled_r2_opt = regex_to_exampled_dnf_regex c r2 rexs in
  begin match (exampled_r1_opt,exampled_r2_opt) with
  | (Some exampled_r1,Some exampled_r2) ->
      let e_o_r1 = to_ordered_exampled_dnf_regex exampled_r1 in
      let e_o_r2 = to_ordered_exampled_dnf_regex exampled_r2 in
      Some (gen_dnf_lens_zipper_internal e_o_r1 e_o_r2)
  | _ -> None
  end
      *)
  (*List.fold_left
    ~f:(fun acc i -> begin match acc with
          | Some _ -> acc
          | None -> gen_dnf_lens_internal c r1 r2 exs i
          end)
    ~init:None
    (range 0 2)*)

