open Core.Std
open Util
open Lang
open Lens
open Eval
open Util
open Pp
open Permutation

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

let rec gen_atom_lens (c:context) (a1:atom) (a2:atom)
                      (exs:examples) : atom_lens option =
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
              begin match gen_dnf_lens c r1 r2 l with
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
                    (exs:examples) : clause_lens option =
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
        let permutations = Permutation.create_all len in
        List.fold_left
        ~f:(fun acc perm ->
          begin match acc with
          | Some _ -> acc
          | None -> let permuted_atoms2 = Permutation.apply_to_list perm atoms2 in
                    let zipped_atoms = List.zip_exn atoms1 permuted_atoms2 in
                    let atom_examples = List.zip_exn left_atom_examples
                      (Permutation.apply_to_list perm right_atom_examples) in
                    let atom_examples_list = List.zip_exn zipped_atoms atom_examples
in
                    let atom_lens_options = List.map
                      ~f:(fun ((c1,c2),(lexs,rexs)) -> gen_atom_lens c c1 c2
                      (List.zip_exn lexs rexs))
                      atom_examples_list in
                    begin match distribute_option atom_lens_options with
                    | None -> None
                    | Some ls -> Some (ls,perm,strings1,strings2)
                    end
          end)
        ~init:None
        permutations
    | (_,_) -> None
    end

and gen_dnf_lens (c:context) (clauses1:dnf_regex) (clauses2:dnf_regex)
                 (exs:examples) : dnf_lens option =
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
        let permutations = Permutation.create_all len in
        List.fold_left
        ~f:(fun acc perm ->
          begin match acc with
          | Some _ -> acc
          | None -> let valid_perm = List.for_all
                      ~f:(fun (lchoice,rchoice) -> Permutation.apply perm
                      lchoice = rchoice)
                      choices in
                    if not valid_perm then None else
                    let (lchoices,rchoices) = List.unzip choices in
                    let lchoice_lexample_pairs = List.zip_exn lexs lchoices in
                    let rchoice_rexample_pairs = List.zip_exn rexs rchoices in
                    let lclauseexs = bucketize_pairs len lchoice_lexample_pairs in
                    let rclauseexs = Permutation.apply_to_list perm
                      (bucketize_pairs len rchoice_rexample_pairs) in
                    let clause_exs = List.zip_exn lclauseexs rclauseexs in
                    let permuted_clauses2 = Permutation.apply_to_list perm clauses2 in
                    let zipped_clauses = List.zip_exn clauses1 permuted_clauses2 in
                    let clause_exs_pairs = List.zip_exn zipped_clauses clause_exs in
                    let clause_lens_options = List.map
                      ~f:(fun ((c1,c2),(lexs,rexs)) -> gen_clause_lens c c1 c2
                        (List.zip_exn lexs rexs))
                      clause_exs_pairs in
                    begin match distribute_option clause_lens_options with
                    | None -> None
                    | Some ls -> Some (ls,perm)
                    end
          end)
        ~init:None
        permutations
    | (_,_) -> None
    end
