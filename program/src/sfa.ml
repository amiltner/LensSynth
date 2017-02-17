open Core.Std
open Util
open Theory
open Datastructures
open String_utilities
open Disjointset
open Partitions
open Arith_theory
open Join_semi_lattice
open Grouping

module SFA = struct
  type state = int

  type ('c,'f) sfa =
    {
      theory      : ('c,'f) theory                                 ;
      states      : state Set.t                                    ;
      transitions : (state, (state,'f) Dictionary.t) Dictionary.t  ;
      initial     : state                                          ;
      finals      : state Set.t                                    ;
    }

  type ('c,'f) state_predicate = ('c, 'f) sfa -> state -> bool
  type ('c,'f) state_predicate_bag = (('c, 'f) state_predicate) Grouping.bag
  type ('c,'f) state_predicate_grouping =
    (('c, 'f) state_predicate) Grouping.grouping

  let create_transition_dictionary
      (theory:('c,'f) theory)
      (transition_list:(state * state * 'u) list)
    : (state, (state,'u) Dictionary.t) Dictionary.t =
    let add_to_transition_dictionary
        (d:(state, (state,'u) Dictionary.t) Dictionary.t)
        ((q1,q2,f):'a * 'a * 'u)
      : (state, (state,'u) Dictionary.t) Dictionary.t =
      let vd =
        begin match Dictionary.find q1 d with
          | None ->
            Dictionary.from_kvp_list
              (=)
              [(q2,f)]
          | Some vd ->
            let f_combined =
              begin match Dictionary.find q2 vd with
                | None -> f
                | Some f' -> theory.or_all [f;f']
              end
            in
            Dictionary.set
              q2
              f_combined
              vd
        end
      in
      Dictionary.set
        q1
        vd
        d
    in
    List.fold_left
      ~f:(fun acc t ->
          add_to_transition_dictionary
            acc
            t)
      ~init:(Dictionary.empty (=))
      transition_list

  let create
      (theory:('c,'f) theory)
      (comparer:'a -> 'a -> bool)
      (states:'a list)
      (transitions:('a * 'a * 'u) list)
      (initial:'a)
      (finals:'a list)
    : ('c,'f) sfa =
    let unique_elt_set = Set.from_list comparer states in

    let elt_pos_pair =
      List.mapi
        ~f:(fun i x -> (x,i))
        (List.rev (Set.as_list unique_elt_set))
    in

    let old_new_mapping =
      Dictionary.from_kvp_list
        comparer
        elt_pos_pair
    in

    let new_states =
      Set.from_list
        (=)
        (Dictionary.vals old_new_mapping)
    in

    let new_transition_list =
      List.map
        ~f:(fun (q1,q2,f) ->
            let q1i = Dictionary.find_exn q1 old_new_mapping in
            let q2i = Dictionary.find_exn q2 old_new_mapping in
            (q1i,q2i,f))
        transitions
    in

    let new_transitions =
      create_transition_dictionary
        theory
        new_transition_list
    in

    let new_initial = Dictionary.find_exn initial old_new_mapping in

    let new_finals =
      Set.from_list
        (=)
        (List.map
           ~f:(fun fin -> Dictionary.find_exn fin old_new_mapping)
           finals)
    in

    {
      theory      = theory          ;
      states      = new_states      ;
      transitions = new_transitions ;
      initial     = new_initial     ;
      finals      = new_finals      ;
    }

  let get_outgoing_transitions
      (d:(state, (state,'u) Dictionary.t) Dictionary.t)
      (q:state)
    : (state, 'u) Dictionary.t =
    begin match Dictionary.find q d with
      | None -> Dictionary.empty (=)
      | Some d -> d
    end

  let evaluate
      (sfa:('c,'f) sfa)
      (string:'c list)
    : bool =
    let rec evaluate_internal (q:state) (string:'c list) : bool =
      begin match string with
        | [] ->
          Set.has q sfa.finals
        | h::t ->
          List.fold_left
            ~f:(fun acc (q',f) ->
                acc ||
                if sfa.theory.evaluate_formula f h then
                  evaluate_internal q' t
                else
                  false)
            ~init:(false)
            (Dictionary.as_kvp_list
               (get_outgoing_transitions sfa.transitions q))
      end
    in
    evaluate_internal sfa.initial string

  let get_transition_list (sfa:('c,'f) sfa) : (state * state * 'u) list =
    List.concat_map
      ~f:(fun (q1,d) ->
          let q2fs = Dictionary.as_kvp_list d in
          List.map ~f:(fun (q2,f) -> (q1,q2,f)) q2fs)
      (Dictionary.as_kvp_list sfa.transitions)

  let quotient (sfa:('c,'f) sfa) (ps:int list list) : ('c,'f) sfa =
    let p_disjointset = List.fold_left
        ~f:(fun acc p ->
            begin match p with
              | [] -> acc
              | h::t ->
                List.fold_left
                  ~f:(fun acc e ->
                      DisjointSet.union_elements acc h e)
                  ~init:acc
                  t
            end)
        ~init:(DisjointSet.empty (=))
        ps
    in
    let disjointset_comparer (i:int) (j:int) =
      let i_rep = DisjointSet.find_representative
          p_disjointset
          i
      in
      let j_rep = DisjointSet.find_representative
          p_disjointset
          j
      in
      i_rep = j_rep
    in

    create
      sfa.theory
      disjointset_comparer
      (Set.as_list sfa.states)
      (get_transition_list sfa)
      sfa.initial
      (Set.as_list sfa.finals)

  let quotient_by_state_predicate_bag
      (sfa:('c,'f) sfa)
      (pb:('c,'f) state_predicate_bag)
    : ('c,'f) sfa =
    let state_predicate_grouping_states =
      List.map
        ~f:(fun q ->
            let predicate_grouping =
              Grouping.grouping
                (fun p -> p sfa q)
                pb
            in
            (q,predicate_grouping))
        (Set.as_list sfa.states)
    in
    let state_predicate_grouping_dict =
      Dictionary.from_kvp_list
        (=)
        state_predicate_grouping_states
    in
    let equivalence_compare
        (q1:state)
        (q2:state)
      : bool =
      let q1_grouping = Dictionary.find_exn q1 state_predicate_grouping_dict in
      let q2_grouping = Dictionary.find_exn q2 state_predicate_grouping_dict in
      Grouping.equal_groupings
        q1_grouping
        q2_grouping
    in
    create
      sfa.theory
      equivalence_compare
      (Set.as_list sfa.states)
      (get_transition_list sfa)
      sfa.initial
      (Set.as_list sfa.finals)

  let determinize (sfa:('c,'f) sfa) : ('c,'f) sfa =
    let new_initial = (Set.from_list (=) [sfa.initial]) in
    let add_if_finals
        (finals:(state Set.t) list)
        (ss:state Set.t)
      : (state Set.t) list =
      let contains_final_config (ss:state Set.t) : bool =
        List.exists
          ~f:(fun s -> Set.has s sfa.finals)
          (Set.as_list ss)
      in
      if contains_final_config ss then
        ss::finals
      else
        finals
    in
    let rec determinize_internal
        (new_states:(state Set.t) list)
        (new_finals:(state Set.t) list)
        (new_transitions:((state Set.t) * (state Set.t) * 'u) list)
        (processed_states:(state Set.t) Set.t)
        (to_visit:(state Set.t) list)
      : ('c,'f) sfa =
      begin match to_visit with
        | [] ->
          create
            sfa.theory
            (Set.equals)
            new_states
            new_transitions
            new_initial
            new_finals
        | h::t ->
          if Set.has h processed_states then
            determinize_internal
              new_states
              new_finals
              new_transitions
              processed_states
              t
          else
            let new_finals = add_if_finals new_finals h in
            let new_states = h::new_states in
            let outgoing_transitions =
              List.concat_map
                ~f:(fun q ->
                    Dictionary.as_kvp_list
                      (get_outgoing_transitions sfa.transitions q))
                (Set.as_list h)
            in
            let (outgoing_states,outgoing_predicates) =
              List.unzip
                outgoing_transitions
            in
            let brand_new_transitions =
              List.map
                ~f:(fun (p,bs) ->
                    let target =
                      List.filter_map
                        ~f:(fun (b,s) ->
                            if b then
                              Some s
                            else
                              None)
                        (List.zip_exn bs outgoing_states)
                    in
                    let outgoing_transition =
                      (h,Set.from_list (=) target,p)
                    in
                    (outgoing_transition))
                (generate_minterms sfa.theory outgoing_predicates)
            in
            let newly_outgoing_states =
              List.map
                ~f:(fun (_,t,_) -> t)
                brand_new_transitions
            in
            let new_transitions = new_transitions@brand_new_transitions in
            let to_visit = newly_outgoing_states@t in
            let processed_states = Set.add h processed_states in
            determinize_internal
              new_states
              new_finals
              new_transitions
              processed_states
              to_visit
      end
    in
    determinize_internal
      []
      []
      []
      (Set.empty (Set.equals))
      [new_initial]

  let reverse_edges
      (sfa:('c,'f) sfa)
    : (state, (state,'u) Dictionary.t) Dictionary.t =
    let transition_list = get_transition_list sfa in
    let transition_reversed_list =
      List.map
        ~f:(fun (q1,q2,f) -> (q2,q1,f))
        transition_list
    in
    create_transition_dictionary sfa.theory transition_reversed_list

  let partition_size_comparison
      (ps:'a Partitions.t)
      (p1:Partitions.p)
      (p2:Partitions.p)
    : comparison =
    let s1 = Partitions.get_elements_exn ps p1 in
    let s2 = Partitions.get_elements_exn ps p2 in
    let s1_size = Set.size s1 in
    let s2_size = Set.size s2 in
    comparison_compare s1_size s2_size

  let minimize (sfa:('c,'f) sfa) : ('c,'f) sfa =
    let sfa = determinize sfa in
    let nonfinals = Set.minus sfa.states sfa.finals in
    let partitions = Partitions.create (=) [sfa.finals;nonfinals] in
    let partition_elts = Partitions.get_all_partitions partitions in
    let minimal_partition =
      min_exn
        (partition_size_comparison partitions)
        partition_elts in
    let queue = [minimal_partition] in
    let reversed_graph = reverse_edges sfa in
    let minimize_internal
        (to_explore:Partitions.p list)
        (partitions:int Partitions.t)
      : int Partitions.t =
      begin match to_explore with
        | [] -> partitions
        | p::_ ->
          let p_set = Partitions.get_elements_exn partitions p in
          let incoming_transitions_list =
            List.concat_map
              ~f:(fun x ->
                  Dictionary.as_kvp_list
                    (get_outgoing_transitions reversed_graph x))
              (Set.as_list p_set)
          in
          let incoming_transitions_dict =
            List.fold_left
              ~f:(fun d (s,f) ->
                  begin match Dictionary.find s d with
                    | None -> Dictionary.set s f d
                    | Some f' ->
                      let f'' = sfa.theory.or_all [f;f'] in
                      Dictionary.set s f'' d
                  end
                )
              ~init:(Dictionary.empty (=))
              incoming_transitions_list
          in
          let preimage_block =
            Set.from_list
              (=)
              (Dictionary.keys incoming_transitions_dict)
          in
          failwith "ah"
      end
    in
    let partitions = minimize_internal queue partitions in
    create
      sfa.theory
      (Partitions.same_partition partitions)
      (Set.as_list sfa.states)
      (get_transition_list sfa)
      sfa.initial
      (Set.as_list sfa.finals)

  (*let ps_with_index = List.mapi ~f:(fun i p -> (i,p)) ps in
    let states = List.map ~f:(fst) ps_with_index in
    let transition (q:state) (q':state) =
    let qs = List.nth_exn ps q in
    let qs' = List.nth_exn ps q' in
    let all_transitions =
      cartesian_map
        (fun q1 q2 -> sfa.transitions q1 q2)
        qs
        qs'
    in
    T.or_all_unary all_transitions
    in
    let initial =
    fst
      (List.find_exn
         ~f:(fun (_,p) -> List.mem p sfa.initial)
         ps_with_index)
    in
    let finals =
    List.filter_map
      ~f:(fun (i,p) ->
          let intersection =
            intersect_lose_order_no_dupes
              (comparison_compare)
              (Set.keys sfa.finals)
              p
          in
          begin match intersection with
            | [] -> Some i
            | _ -> None
          end)
      ps_with_index
    in
    {
    states     = Set.from_list (=) states ;
    transition = transition               ;
    initial    = initial                  ;
    finals     = Set.from_list (=) finals ;
    }*)

  let equals (sfa1:('c,'f) sfa) (sfa2:('c,'f) sfa) : bool =
    (Set.equals sfa1.states sfa2.states) &&
    (get_transition_list sfa1 = get_transition_list sfa2) &&
    (sfa1.initial = sfa2.initial) &&
    (Set.equals sfa1.finals sfa2.finals)

  let to_string (sfa:('c,'f) sfa) : string =
    let states_string =
      string_of_list
        string_of_int
        (Set.as_list sfa.states)
    in
    let inner_dict_converter d =
      string_of_list
        (string_of_double
           string_of_int
           sfa.theory.to_string)
        (Dictionary.as_kvp_list d)
    in
    let transitions_string =
      string_of_list 
        (string_of_double
           string_of_int
           inner_dict_converter)
        (Dictionary.as_kvp_list sfa.transitions)
    in
    let initial_string =
      string_of_int sfa.initial
    in
    let finals_string =
      string_of_list
        string_of_int
        (Set.as_list sfa.finals)
    in
    "States: " ^ states_string ^
    "\nTransitions: " ^ transitions_string ^
    "\nInitial: " ^ initial_string ^
    "\nFinals: " ^ finals_string





  type combined_state = state * state
  (* require theories are the same, as we could like satisfiability checks *)
  let producted_automaton
      (sfa1:('c,'f) sfa)
      (sfa2:('d,'g) sfa)
      (formula_combiner:'f -> 'g -> 'h)
      (combined_theory:('e,'h) theory)
    : ('e,'h) sfa =
    let sfa1_state_list = Set.as_list sfa1.states in
    let sfa2_state_list = Set.as_list sfa2.states in
    let sfa1_finals_list = Set.as_list sfa1.finals in
    let sfa2_finals_list = Set.as_list sfa2.finals in
    let combined_states =
      cartesian_product
        sfa1_state_list
        sfa2_state_list
    in
    let combined_initial = (sfa1.initial,sfa2.initial) in
    let combined_finals =
      cartesian_product
        sfa1_finals_list
        sfa2_finals_list
    in
    let rec transition_builder
        (state_queue:combined_state list)
        (processed_states:combined_state Set.t)
        (transition_acc:(combined_state * combined_state * 'u) list)
      : (combined_state * combined_state * 'u) list =
      begin match state_queue with
        | [] -> transition_acc
        | combined_state::state_queue' ->
          if Set.has combined_state processed_states then
            transition_builder
              state_queue'
              processed_states
              transition_acc
          else
            let (u_s,b_s) = combined_state in
            let u_s_transitions =
              begin match Dictionary.find u_s sfa1.transitions with
                | None -> []
                | Some d -> Dictionary.as_kvp_list d
              end
            in
            let b_s_transitions =
              begin match Dictionary.find b_s sfa2.transitions with
                | None -> []
                | Some d -> Dictionary.as_kvp_list d
              end
            in
            let combined_transitions =
              cartesian_map
                (fun (u_t,u_f) (b_t,b_f) ->
                   let target_combined_state = (u_t,b_t) in
                   let combined_transition =
                     formula_combiner
                       u_f
                       b_f
                   in
                   (combined_state,target_combined_state,combined_transition))
                u_s_transitions
                b_s_transitions
            in
            let all_targets =
              List.map
                ~f:(fun (_,t,_) -> t)
                combined_transitions
            in
            let to_enqueue =
              List.filter
                ~f:(fun t -> not (Set.has t processed_states))
                all_targets
            in
            transition_builder
              (to_enqueue@state_queue')
              (Set.add combined_state processed_states)
              (combined_transitions@transition_acc)
      end
    in
    let combined_transitions =
      transition_builder
        [combined_initial]
        (Set.empty (=))
        []
    in
    create
      combined_theory
      (=)
      combined_states
      combined_transitions
      combined_initial
      combined_finals




  let symbolic_application
      (theory:('c,'u,'b) unary_binary_theory)
      (sfa:('c,'u) sfa)
      (sft:(('c*'c),'b) sfa)
    : ('c,'u) sfa =
    let application_combiner
        (u_f:'u)
        (b_f:'b)
        : 'u =
      theory.app_unary_to_first_binary
        u_f
        b_f
    in
    let combined_theory = sfa.theory in
    producted_automaton
      sfa
      sft
      application_combiner
      combined_theory

  let intersect_automaton
      (sfa1:('c,'f) sfa)
      (sfa2:('c,'g) sfa)
      (satisfiability_checker:'f -> 'g -> bool)
    : ('c,('f * 'g)) sfa =
    let combined_theory =
      product_theory
        sfa1.theory
        sfa2.theory
        satisfiability_checker
    in
    producted_automaton
      sfa1
      sfa2
      pair_theory_formula_combiner
      combined_theory

  let map_edges
      (theory:('c,'g) theory)
      (edge_function:'f -> 'g)
      (sfa:('c,'f) sfa)
    : ('c,'g) sfa =
    let states = sfa.states in
    let transition_list = get_transition_list sfa in
    let new_transition_list =
      List.map
        ~f:(fun (s1,s2,f) -> (s1,s2,edge_function f))
        transition_list
    in
    let transitions =
      create_transition_dictionary
        theory
        new_transition_list
    in
    let initial = sfa.initial in
    let finals = sfa.finals in
    {
      theory      = theory      ;
      states      = states      ;
      transitions = transitions ;
      initial     = initial     ;
      finals      = finals      ;
    }

  let filter_edges
      (edge_filter:'f -> bool)
      (sfa:('c,'f) sfa)
    : ('c,'f) sfa =
    let theory = sfa.theory in
    let states = sfa.states in
    let transition_list = get_transition_list sfa in
    let new_transition_list =
      List.filter
        ~f:(fun (_,_,f) -> edge_filter f)
        transition_list
    in
    let transitions =
      create_transition_dictionary
        theory
        new_transition_list
    in
    let initial = sfa.initial in
    let finals = sfa.finals in
    {
      theory      = theory      ;
      states      = states      ;
      transitions = transitions ;
      initial     = initial     ;
      finals      = finals      ;
    }
    
  let apply_semilattice
      (semi_lattice:'f join_semi_lattice)
      (sfa:('c,'f) sfa)
    : ('c,'f) sfa =
    map_edges
      sfa.theory
      semi_lattice
      sfa

  let retrieve_nonrepeating_paths
      (sfa:('c,'f) sfa)
    : ((state * state * 'f) list) list =
    let rec retrieve_nonrepeating_paths_internal
        ((path_acc,nodes_passed):((state * state * 'f) list * state Set.t))
        (cur_state:state)
      : ((state * state * 'f) list) list =
      if Set.has cur_state nodes_passed then
        []
      else
        let outgoing_transition_list =
          Dictionary.as_kvp_list
            (get_outgoing_transitions
               sfa.transitions
               cur_state)
        in
        let nodes_passed' = Set.add cur_state nodes_passed in
        let further_paths =
          List.concat_map
            ~f:(fun (s',f') ->
                let path_acc' = (cur_state,s',f')::path_acc in
                retrieve_nonrepeating_paths_internal
                  (path_acc',nodes_passed')
                  s')
            outgoing_transition_list
        in
        if Set.has cur_state sfa.finals then
          path_acc::further_paths
        else
          further_paths
    in
    retrieve_nonrepeating_paths_internal ([],Set.empty (=)) sfa.initial

  let filter_unsat_edges
      (sfa:('c,'f) sfa)
    : ('c,'f) sfa =
    filter_edges
      (fun f -> sfa.theory.is_satisfiable f)
      sfa

  let is_empty
      (sfa:('c,'f) sfa)
    : bool =
    let filtered_sfa = filter_unsat_edges sfa in
    List.is_empty (retrieve_nonrepeating_paths filtered_sfa)
end

type arith_sfa = (ArithTheory.c,ArithTheory.u) SFA.sfa
type arith_sft =
  (ArithTheory.c * ArithTheory.c
  ,ArithTheory.b)
    SFA.sfa
