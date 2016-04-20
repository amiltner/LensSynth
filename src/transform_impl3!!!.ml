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

let rec calculate_userdef_distribution (r:regex) : ((string * int) * int) list =
  let rec calculate_atom_userdef_distribution (a:atom)
                                              (depth:int)
                                              (acc:(string * int) list)
                                              : (string * int) list =
    begin match a with
    | AUserDefined s -> [(s,depth)]
    | AStar r' -> calculate_dnf_userdef_distribution r' (depth+1) acc
    end
  and calculate_clause_userdef_distribution ((atoms,strings):clause)
                                            (depth:int) 
                                            (acc:(string * int) list)
                                            : (string * int) list =
    List.fold_left
      ~f:(fun acc a ->
        calculate_atom_userdef_distribution a depth acc)
      ~init:acc
      atoms
  and calculate_dnf_userdef_distribution (clauses:dnf_regex)
                                         (depth:int)
                                         (acc:(string * int) list)
                                         : (string * int) list =
    List.fold_left
      ~f:(fun acc c ->
          (calculate_clause_userdef_distribution c depth acc))
      ~init:acc
      clauses
  in
  let rec retrieve_counts_in_ordered_list (counts:'a list)
                                          (acc:('a * int) list)
                                          : ('a * int) list =
    begin match counts with
    | [] -> acc
    | h::t ->
        retrieve_counts_in_ordered_list
          t
          (begin match acc with
          | [] -> [(h,1)]
          | (hd,i)::tl ->
              begin match comparison_compare hd h with
              | EQ -> (hd,i+1)::tl
              | _ -> (h,1)::acc
              end
          end)
    end
  in

  let data_list = calculate_dnf_userdef_distribution (to_dnf_regex r) 0 [] in
  let ordered_data_list = List.sort ~cmp:compare data_list in
  List.rev (retrieve_counts_in_ordered_list ordered_data_list [])

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
  print_endline "here slow!";
  print_endline (Pp.pp_regexp r1);
  print_endline (Pp.pp_regexp r2);
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
  let userdef_dist_r1 = 
    (calculate_userdef_distribution r1) in
  let userdef_dist_r2 = 
    (calculate_userdef_distribution r2) in
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
