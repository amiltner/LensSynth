open Core.Std
open Counters
open Fasteval
open Util
open Lang
open Lens
open Eval
open Util
open Pp
open Permutation

let empty_string = RegExBase ""

let rec exponentiate (r:regex) (n:int) : regex =
  if n < 0 then
    failwith "invalid exponential"
  else if n = 0 then
    empty_string
  else
    RegExConcat
      (exponentiate r (n-1)
      ,r)

let rec calculate_userdef_distribution (r:regex) : ((string * int) Counters.t) * int =
  let rec calculate_userdef_distribution_internal (r:regex) (depth:int)
    : ((string * int) Counters.t) * int =
      begin match r with
      | RegExBase _ -> (Counters.create comparison_compare,1)
      | RegExUserDefined s ->
            (Counters.add
                         (Counters.create (comparison_compare))
                         (s,depth),1)
      | RegExOr (r1,r2) ->
          let (counters_r1,width1) =
            calculate_userdef_distribution_internal r1 depth in
          let (counters_r2,width2) =
            calculate_userdef_distribution_internal r2 depth in
          (Counters.merge (fun x y -> x + y) counters_r1 counters_r2,width1+width2)
      | RegExConcat (r1,r2) ->
          let (counters_r1,width1) =
            calculate_userdef_distribution_internal r1 depth in
          let (counters_r2,width2) =
            calculate_userdef_distribution_internal r2 depth in
          (Counters.merge (fun x y -> (x*width2 + y*width1)) counters_r1 counters_r2,width1*width2)
      | RegExStar r' ->
          (fst (calculate_userdef_distribution_internal r' (depth+1)),1)
      end
  in
  calculate_userdef_distribution_internal r 0
  (*let rec calculate_atom_userdef_distribution (a:atom) (depth:int) : (string * int) Counters.t =
    begin match a with
    | AUserDefined s ->
        Counters.add
          (Counters.create (comparison_compare))
          (s,depth)
    | AStar r' -> calculate_dnf_userdef_distribution r' (depth+1)
    end
  and calculate_clause_userdef_distribution ((atoms,strings):clause) (depth:int) : (string * int) Counters.t =
    List.fold_left
      ~f:(fun acc a ->
        Counters.merge
          acc
          (calculate_atom_userdef_distribution a depth))
      ~init:(Counters.create comparison_compare)
      atoms
  and calculate_dnf_userdef_distribution (clauses:dnf_regex) (depth:int) : (string * int) Counters.t =
    List.fold_left
      ~f:(fun acc c ->
        Counters.merge
          acc
          (calculate_clause_userdef_distribution c depth))
      ~init:(Counters.create comparison_compare)
      clauses
  in

  calculate_dnf_userdef_distribution (to_dnf_regex r) 0*)

  (*begin match r with
  | RegExBase _ -> Counters.create (comparison_compare)
  | RegExOr (r1,r2) ->
      let r1_counter = calculate_userdef_distribution r1 in
      let r2_counter = calculate_userdef_distribution r2 in
      Counters.merge r1_counter r2_counter
  | RegExConcat (r1,r2) ->
      let r1_counter = calculate_userdef_distribution r1 in
      let r2_counter = calculate_userdef_distribution r2 in
      Counters.merge r1_counter r2_counter
  | RegExStar (r') -> calculate_userdef_distribution r'
  | RegExUserDefined s ->
      Counters.add
        (Counters.create (comparison_compare))
        s
  end*)

let retrieve_priority (r1:regex) (r2:regex) : float =
  let rec retrieve_priority_internal (cs1:((string * int) * int) list)
                                 (cs2:((string * int) * int) list)
                                 : float =
    begin match (cs1,cs2) with
    | ((s1,c1)::t1,(s2,c2)::t2) ->
        begin match comparison_compare s1 s2 with
        | EQ -> (2.0 ** (Float.abs ((Float.of_int c1) -. (Float.of_int c2)))) *.
                (retrieve_priority_internal t1 t2)
        | LT -> (2.0 ** (Float.of_int c1)) *.
                (retrieve_priority_internal t1 cs2)
        | GT -> (2.0 ** (Float.of_int c2)) *.
                (retrieve_priority_internal cs1 t2)
        end
    | (_,[]) ->
        List.fold_left
        ~f:(fun acc (_,c) -> (2.0 ** (Float.of_int c)) *. acc)
        ~init:1.0
        cs1
    | ([],_) ->
        List.fold_left
        ~f:(fun acc (_,c) -> (2.0 ** (Float.of_int c)) *. acc)
        ~init:1.0
        cs2
    end
  in
  let userdef_dist_r1 = Counters.as_ordered_assoc_list (fst (
    (calculate_userdef_distribution r1)) )in
  let userdef_dist_r2 = Counters.as_ordered_assoc_list (fst (
    (calculate_userdef_distribution r2)) )in
  let ans = (retrieve_priority_internal
    userdef_dist_r1
    userdef_dist_r2)
    (**. (2.0 ** (Float.of_int (size r1)))*)
    (**. (2.0 ** (Float.of_int (size r2)))*)
    (**. (Float.of_int (or_size r1))
    *. (Float.of_int (or_size r2))*)
    *. (2.0 ** (Float.of_int (abs ((or_size r1) - (or_size r2)))))
    (**. (4.0 ** (Float.of_int (List.length userdef_dist_r1)))
    *. (4.0 ** (Float.of_int (List.length userdef_dist_r2)))*) in
        (*print_endline "\n\n\nr1";
        print_endline (Pp.pp_regexp r1);
        print_endline "\nr2";
        print_endline (Pp.pp_regexp r2);
        print_endline (Counters.pp (fun (s,i) -> "(" ^ (string_of_int i) ^ "," ^
        s ^ ")")
        (calculate_userdef_distribution r1) );*)
        (*print_endline (string_of_int (or_size r1));
        print_endline (string_of_int (or_size r2));*)
        (*print_endline ("priority_internal" ^ (Float.to_string (retrieve_priority_internal
    userdef_dist_r1
    userdef_dist_r2)));*)
        (*print_endline ("total:" ^ (Float.to_string ans));*)
  ans

let rec quotiented_star (r:regex) (n:int) : regex =
  if n < 1 then
    failwith "invalid modulation"
  else if n = 1 then
    empty_string
  else
    RegExOr
      ((quotiented_star r (n-1))
      ,(exponentiate r (n-1)))

let rec empty_or_not_star_expansion (r:regex) : regex =
  RegExOr
    (RegExBase ""
    ,RegExConcat
      (r
      ,RegExStar r))

let rec quotient_product_expansion (n:int) (r:regex) : regex =
  RegExConcat
    ((quotiented_star r n)
    ,(RegExStar
      (exponentiate r n)))

let rec expand_stars (r:regex) (n:int) (max_size:int) : regex list =
    if n = 0 then
      [r]
    else
      let relevant_primes = primes_beneath_n max_size in
      let transformations = empty_or_not_star_expansion::
        (List.map
          ~f:(fun p -> quotient_product_expansion p)
          relevant_primes) in
      let rec expand_stars_internal (r:regex) : regex list =
        begin match r with
        | RegExBase _ -> []
        | RegExConcat (r1,r2) ->
            let r1_expansions = expand_stars_internal r1 in
            let r2_expansions = expand_stars_internal r2 in
            (List.map
              ~f:(fun expansion -> RegExConcat (expansion,r2))
              r1_expansions)
            @
            (List.map
              ~f:(fun expansion -> RegExConcat (r1,expansion))
              r2_expansions)
        | RegExOr (r1,r2) ->
            let r1_expansions = expand_stars_internal r1 in
            let r2_expansions = expand_stars_internal r2 in
            (List.map
              ~f:(fun expansion -> RegExOr (expansion,r2))
              r1_expansions)
            @
            (List.map
              ~f:(fun expansion -> RegExOr (r1,expansion))
              r2_expansions)
        | RegExStar (r') ->
            List.map ~f:(fun t -> t r') transformations
            @ (List.map ~f:(fun r'' -> RegExStar r'') (expand_stars_internal r'))
        | RegExUserDefined _ -> []
        end
      in
      List.fold_left
      ~f:(fun acc k ->
        List.dedup
          (List.concat (List.map ~f:(fun x -> expand_stars_internal x) acc)))
      ~init:[r]
      (range 0 (n-1))

let rec expand_userdefs (c:context) (r:regex)
                            : regex list =
  begin match r with
  | RegExBase _ -> []
  | RegExConcat (r1,r2) ->
      let r1_expansions = expand_userdefs c r1 in
      let r2_expansions = expand_userdefs c r2 in
      (List.map
        ~f:(fun expansion -> RegExConcat (expansion,r2))
        r1_expansions)
      @
      (List.map
        ~f:(fun expansion -> RegExConcat (r1,expansion))
        r2_expansions)
  | RegExOr (r1,r2) ->
      let r1_expansions = expand_userdefs c r1 in
      let r2_expansions = expand_userdefs c r2 in
      (List.map
        ~f:(fun expansion -> RegExOr (expansion,r2))
        r1_expansions)
      @
      (List.map
        ~f:(fun expansion -> RegExOr (r1,expansion))
        r2_expansions)
  | RegExStar (r') ->
      List.map
        ~f:(fun expansion -> RegExStar expansion)
        (expand_userdefs c r')
  | RegExUserDefined t ->
      begin match List.Assoc.find c t with
      | Some rex -> [rex]
      | None -> []
      end
  end

let rec expand_required_expansions (c:context) (r1:regex) (r2:regex)
                            : (regex * regex) option =
  let rec retrieve_transitive_userdefs (r:regex) : string list =
    begin match r with
    | RegExBase _ -> []
    | RegExConcat (r1,r2) -> (retrieve_transitive_userdefs r1) @
        (retrieve_transitive_userdefs r2)
    | RegExOr (r1,r2) -> (retrieve_transitive_userdefs r1) @
        (retrieve_transitive_userdefs r2)
    | RegExStar r' -> retrieve_transitive_userdefs r'
    | RegExUserDefined t -> t::
      (begin match List.Assoc.find c t with
      | None -> []
      | Some rex -> retrieve_transitive_userdefs rex
      end)
    end
  in
  let rec expand_required_expansions (bad_userdefs:string list)
                                 (r:regex)
                                 : regex option =
    begin match r with
    | RegExBase _ -> Some r
    | RegExConcat (r1,r2) -> 
        begin match (expand_required_expansions bad_userdefs r1,
                     expand_required_expansions bad_userdefs r2) with
        | (Some r1', Some r2') -> Some (RegExConcat (r1',r2'))
        | _ -> None
        end
    | RegExOr (r1,r2) -> 
        begin match (expand_required_expansions bad_userdefs r1,
                     expand_required_expansions bad_userdefs r2) with
        | (Some r1', Some r2') -> Some (RegExOr (r1',r2'))
        | _ -> None
        end
    | RegExStar r' ->
        begin match expand_required_expansions bad_userdefs r' with
        | Some r'' -> Some (RegExStar r'')
        | _ -> None
        end
    | RegExUserDefined t ->
        if List.mem bad_userdefs t then
          begin match List.Assoc.find c t with
          | None -> None
          | Some r' -> expand_required_expansions bad_userdefs r'
          end
        else
          Some r
    end
  in

  let r1_transitive_userdefs = retrieve_transitive_userdefs r1 in
  let r2_transitive_userdefs = retrieve_transitive_userdefs r2 in
  let r1trans_not_in_r2trans = set_minus_lose_order comparison_compare
      r1_transitive_userdefs
      r2_transitive_userdefs in
  let r2trans_not_in_r1trans = set_minus_lose_order comparison_compare
      r2_transitive_userdefs
      r1_transitive_userdefs in
  begin match (expand_required_expansions r1trans_not_in_r2trans r1,
               expand_required_expansions r2trans_not_in_r1trans r2) with
  | (Some r1', Some r2') -> Some (r1',r2')
  | _ -> None
  end

let rec expand_once (max_size:int) (c:context) (r1:regex) (r2:regex)
                            : (regex * regex) list =
  let max_size = min (max (or_size r1) (or_size r2)) max_size in
  let left_expansions = (expand_stars r1 1 max_size) @ (expand_userdefs c r1) in
  let right_expansions = (expand_stars r2 1 max_size) @ (expand_userdefs c r2) in
  List.dedup
  (((List.map ~f:(fun le -> (le,r2)) left_expansions)
  @ (List.map ~f:(fun re -> (r1,re)) right_expansions)))

let rec apply_transformations (max_size:int) (c:context) (r1:regex) (r2:regex) (n:int)
                            : (regex * regex) list =
    List.fold_left
      ~f:(fun acc _ ->
        List.concat_map ~f:(fun (x,y) -> expand_once max_size c x y) acc)
      ~init:[(r1,r2)]
      (range 0 (n-1))
  (*let max_size = max (size r1) (size r2) in
  let splits = List.map ~f:(fun m -> (m,n-m)) (range 0 n) in
  List.concat_map
    ~f:(fun (ln,rn) ->
      let left_exps = expand_stars r1 ln max_size in
      let right_exps = expand_stars r2 rn max_size in
      cartesian_map (fun x y -> (x,y)) left_exps right_exps)
    splits*)


let rec atom_lens_to_lens (a:atom_lens) : lens =
  begin match a with
  | AIterate d -> IterateLens (dnf_lens_to_lens d)
  | AIdentity -> IdentityLens
  end

and clause_lens_to_lens ((atoms,permutation,strings1,strings2):clause_lens)
  : lens =
    let rec combine_scct_and_atom_lenses
            (atom_lenses:lens list)
            (scct:swap_concat_compose_tree)
            : (lens * lens list) =
      begin match scct with
      | SCCTSwap (s1,s2) ->
          let (l1,remaining_left) =
            combine_scct_and_atom_lenses
              atom_lenses
              s1 in
          let (l2,remaining_total) =
            combine_scct_and_atom_lenses
              remaining_left
              s2 in
          (SwapLens(l1,l2),remaining_total)
      | SCCTConcat (s1,s2) ->
          let (l1,remaining_left) =
            combine_scct_and_atom_lenses
              atom_lenses
              s1 in
          let (l2,remaining_total) =
            combine_scct_and_atom_lenses
              remaining_left
              s2 in
          (ConcatLens(l1,l2),remaining_total)
      | SCCTCompose (s1,s2) ->
          let s2size = size_scct s2 in
          let identity_copies = duplicate IdentityLens s2size in
          let (l1,_) =
            combine_scct_and_atom_lenses
              identity_copies
              s1 in
          let (l2,remaining_total) =
            combine_scct_and_atom_lenses
              atom_lenses
              s2 in
          (ComposeLens(l1,l2),remaining_total)
      | SCCTLeaf -> split_by_first_exn atom_lenses
      end
    in
    let string_combos = List.zip_exn strings1 strings2 in
    let string_combo_lss = List.map ~f:(fun (x,y) -> ConstLens (x,y)) string_combos in
    let (string_combos_lss_hd,string_combos_lss_tl) =
      split_by_first_exn string_combo_lss in
    let atom_lenses =
      List.map ~f:atom_lens_to_lens atoms in
    let atom_string_zips = List.zip_exn atom_lenses string_combos_lss_tl in
    let atom_string_concats =
      List.map ~f:(fun (x,y) -> ConcatLens (x,y)) atom_string_zips in
    begin match atom_string_concats with
    | [] -> string_combos_lss_hd
    | _ ->
      let permutation_scct =
        Permutation.to_swap_concat_compose_tree permutation in
      ConcatLens(string_combos_lss_hd,
        (fst (combine_scct_and_atom_lenses
          atom_string_concats
          permutation_scct)))
    end


and dnf_lens_to_lens ((clauses,permutation):dnf_lens) : lens =
  let rec combine_scct_and_clause_lenses
          (clause_lenses:lens list)
          (scct:swap_concat_compose_tree)
          : (lens * lens list) =
    let (hd,tl) = split_by_first_exn clause_lenses in
    (List.fold_left
    ~f:(fun acc l -> UnionLens (acc, l))
    ~init:hd
    tl,[])
    (*begin match scct with
    | SCCTSwap (s1,s2) ->
        let (l1,remaining_left) =
          combine_scct_and_clause_lenses
            clause_lenses
            s1 in
        let (l2,remaining_total) =
          combine_scct_and_clause_lenses
            remaining_left
            s2 in
        (SwapLens(l1,l2),remaining_total)
    | SCCTConcat (s1,s2) ->
        let (l1,remaining_left) =
          combine_scct_and_clause_lenses
            clause_lenses
            s1 in
        let (l2,remaining_total) =
          combine_scct_and_clause_lenses
            remaining_left
            s2 in
        (ConcatLens(l1,l2),remaining_total)
    | SCCTCompose (s1,s2) ->
        let s2size = size_scct s2 in
        let identity_copies = duplicate IdentityLens s2size in
        let (l1,_) =
          combine_scct_and_clause_lenses
            identity_copies
            s1 in
        let (l2,remaining_total) =
          combine_scct_and_clause_lenses
            clause_lenses
            s2 in
        (ComposeLens(l1,l2),remaining_total)
    | SCCTLeaf -> split_by_first_exn clause_lenses
    end*)
  in
  let clause_lenses =
    List.map ~f:clause_lens_to_lens clauses in
  begin match clause_lenses with
  | [] -> IdentityLens
  | _ ->
    let permutation_scct =
      Permutation.to_swap_concat_compose_tree permutation in
        fst (combine_scct_and_clause_lenses
          clause_lenses
          permutation_scct)
  end

let rec simplify_lens (l:lens) : lens =
  let rec is_leftmost_all_concats_identity (l:lens) : bool =
    begin match l with
    | ConcatLens (l1,l2) -> is_leftmost_all_concats_identity l1
    | IdentityLens -> true
    | _ -> false
    end
  in
  let rec is_rightmost_all_concats_identity (l:lens) : bool =
    begin match l with
    | ConcatLens (l1,l2) -> is_leftmost_all_concats_identity l2
    | IdentityLens -> true
    | _ -> false
    end
  in
  let rec contains_ored_identity (l:lens) : bool =
    begin match l with
    | UnionLens (l1,l2) ->
        (contains_ored_identity l1) ||
          (contains_ored_identity l2)
    | IdentityLens -> true
    | _ -> false
    end
  in
  let rec remove_rightmost_all_concats_identity (l:lens) : lens =
    begin match l with
    | ConcatLens (l1,IdentityLens) -> l1
    | ConcatLens (l1,l2) ->
        ConcatLens (l1,remove_rightmost_all_concats_identity l2)
    | _ -> failwith "bad input"
    end
  in
  let rec try_remove_leftmost_concats_const
          (l:lens)
          : (lens * string * string) option =
    begin match l with
    | ConcatLens (ConstLens(s1,s2),l2) ->
        Some (l2,s1,s2)
    | ConcatLens (l1,l2) ->
        Option.map
          ~f:(fun (l1',s1,s2) -> (ConcatLens (l1',l2),s1,s2))
          (try_remove_leftmost_concats_const l1)
    | _ -> None
    end
  in
  let rec try_replace_leftmost_concats_const
          (l:lens)
          (s1:string)
          (s2:string)
          : lens option =
    begin match l with
    | ConcatLens (l1,l2) ->
        Option.map
          ~f:(fun x -> ConcatLens (x,l2))
          (try_replace_leftmost_concats_const l1 s1 s2)
    | ConstLens (s1',s2') -> Some (ConstLens (s1^s1',s2^s2'))
    | _ -> None
    end
  in
  let rec try_replace_rightmost_concats_const
          (l:lens)
          (s1:string)
          (s2:string)
          : lens option =
    begin match l with
    | ConcatLens (l1,l2) ->
        Option.map
          ~f:(fun x -> ConcatLens (l1,x))
          (try_replace_rightmost_concats_const l2 s1 s2)
    | ConstLens (s1',s2') -> Some (ConstLens (s1^s1',s2^s2'))
    | _ -> None
    end
  in

  let ans = begin match l with
    | ConstLens (s1,s2) ->
        if s1 = s2 then
          IdentityLens
        else
          l
    | ConcatLens (l1,l2) ->
        let l1 = simplify_lens l1 in
        let l2 = simplify_lens l2 in
        begin match (l1,l2) with
        | (IdentityLens,l2) ->
            if is_leftmost_all_concats_identity l2 then
              l2
            else
              ConcatLens (l1,l2)
        | (l1,IdentityLens) ->
            if is_rightmost_all_concats_identity l1 then
              l1
            else
              ConcatLens (l1,l2)
        | (ConstLens(s1,s2),l2) ->
            begin match try_replace_leftmost_concats_const l2 s1 s2 with
            | None -> ConcatLens(l1,l2)
            | Some l' -> l'
            end
        | (l1,ConstLens(s1,s2)) ->
            begin match try_replace_rightmost_concats_const l1 s1 s2 with
            | None -> ConcatLens(l1,l2)
            | Some l' -> l'
            end
        | _ ->
            if is_leftmost_all_concats_identity l2
              && is_rightmost_all_concats_identity l1 then
                ConcatLens (remove_rightmost_all_concats_identity l1,l2)
            else
              begin match try_remove_leftmost_concats_const l2 with
              | Some (l2',s1,s2) ->
                  begin match try_replace_rightmost_concats_const l1 s1 s2 with
                  | Some l1' -> ConcatLens (l1',l2')
                  | None -> ConcatLens (l1,l2)
                  end
              | None -> ConcatLens (l1,l2)
              end
        end
    | UnionLens (l1,l2) ->
        let l1 = simplify_lens l1 in
        let l2 = simplify_lens l2 in
        if l1 = IdentityLens && contains_ored_identity l2 then
          l2
        else if l2 = IdentityLens && contains_ored_identity l1 then
          l1
        else
          UnionLens (l1,l2)
    | SwapLens (l1,l2) ->
        SwapLens (simplify_lens l1,simplify_lens l2)
    | ComposeLens (l1,l2) ->
        let l1 = simplify_lens l1 in
        let l2 = simplify_lens l2 in
        if l1 = IdentityLens then
          l2
        else if l2 = IdentityLens then
          l1
        else
          ComposeLens (l1,l2)
    | IterateLens l' ->
        let l' = simplify_lens l' in
        if l' = IdentityLens then
          IdentityLens
        else
          IterateLens l'
    | IdentityLens -> IdentityLens
    end
  in
  if ans = l then
    ans else 
  simplify_lens ans



