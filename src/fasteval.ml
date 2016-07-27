open Lang
open Util
open Pp
open Core.Std
open Regexcontext

let rec to_empty_exampled_regex (r:regex) : exampled_regex =
  begin match r with
  | RegExEmpty -> ERegExEmpty
  | RegExBase s -> ERegExBase (s,[])
  | RegExConcat (r1,r2) ->
      ERegExConcat (to_empty_exampled_regex r1,to_empty_exampled_regex r2,[])
  | RegExOr (r1,r2) ->
      ERegExOr
        ((to_empty_exampled_regex r1),
         (to_empty_exampled_regex r2),
         [])
  | RegExStar r' -> ERegExStar (to_empty_exampled_regex r',[])
  | RegExUserDefined t -> ERegExUserDefined (t,[],[])
  | RegExMappedUserDefined t -> ERegExMappedUserDefined (t,[],[])
  end

type data = string * exampled_regex *
            (string -> exampled_regex -> exampled_regex) list
            * int list * string option

type state =
  | State of (data -> ((state ref * data) list))
  | QAccept

type dfa = (state ref) * (state ref)

let rec regex_to_dfa (c:RegexContext.t) (mcs:mapsbetweencontextside) (r:regex) (inside_userdef:bool) : dfa =
  begin match r with
  | RegExEmpty ->
      let final = ref QAccept in
      (ref (State (fun _ -> [])), final)
  | RegExBase s ->
      let final = ref QAccept in
      (ref (State (fun (str,er,recombiners,is,so) ->
        begin match String.chop_prefix ~prefix:s str with
        | None -> []
        | Some str' ->
            if not inside_userdef then
              begin match er with
              | ERegExBase (b,il) -> [(final,(str',ERegExBase (b,is::il),recombiners,is,so))]
              | _ -> failwith "bad";
              end
            else
              [(final,(str',er,recombiners,is,so))]
        end)), final)
  | RegExConcat (r1,r2) ->
      let (r1_start_ref,r1_end_ref) = regex_to_dfa c mcs r1 inside_userdef in
      let (r2_start_ref,r2_end_ref) = regex_to_dfa c mcs r2 inside_userdef in
      let new_start_fun = (fun (s,er,rc,is,so) ->
        if not inside_userdef then
          begin match er with
          | ERegExConcat (er1,er2,_) ->
            let rc_swapsecond = (fun _ _ -> er2) in
            [r1_start_ref, (s,er1,rc_swapsecond::rc,is,so)]
          | _ -> failwith (Pp.pp_exampled_regex er)
          end
        else
          [r1_start_ref, (s,er,rc,is,so)]) in
      let new_start = State new_start_fun in
      let new_middle_fun = (fun (s,er,rc,is,so) ->
        if not inside_userdef then
          begin match rc with
          | h::t -> let rc_rememberfirst = (fun _ er' -> ERegExConcat (er,er',
          extract_example_list er')) in
            [r2_start_ref, (s,h s er,rc_rememberfirst::t,is,so)]
          | [] -> failwith "stupid bad"
          end
        else
          [r2_start_ref, (s,er,rc,is,so)])
      in
      let middle_state = State new_middle_fun in
      r1_end_ref := middle_state;
      let new_end_ref = ref QAccept in
      let new_r2_end =
        if not inside_userdef then
          State
            (fun (s,er,rc,is,so) ->
              begin match rc with
              | h::t -> [(new_end_ref,(s,h s er, t, is,so))]
              | _ -> failwith "bad coder"
              end)
        else
          State (fun x -> [(new_end_ref,x)])
      in
      r2_end_ref := new_r2_end;
      (ref new_start,new_end_ref)
  | RegExOr (r1,r2) ->
      let (r1_start_ref,r1_end_ref) = regex_to_dfa c mcs r1 inside_userdef in
      let (r2_start_ref,r2_end_ref) = regex_to_dfa c mcs r2 inside_userdef in
      let new_start_fun = (fun (s,er,rc,is,so) ->
        if not inside_userdef then
          begin match er with
          | ERegExOr (er1,er2,il) ->
              let rc_left = 
                (fun _ er1' ->
                  ERegExOr (er1',er2,is::il)) in
              let rc_right =
                (fun _ er2' ->
                  ERegExOr (er1,er2',is::il)) in
              [(r1_start_ref, (s,er1,rc_left::rc,is,so))
              ;(r2_start_ref, (s,er2,rc_right::rc,is,so))]
          | _ -> failwith (Pp.pp_exampled_regex er)
          end
        else
          [(r1_start_ref, (s,er,rc,is,so))
          ;(r2_start_ref, (s,er,rc,is,so))]
        ) in
      let new_start = State (new_start_fun) in
      let new_end_ref = ref QAccept in
      let new_inner_end =
        if not inside_userdef then
          State
            (fun (s,er,rc,is,so) ->
              begin match rc with
              | h::t -> [(new_end_ref,(s,h s er, t, is,so))]
              | _ -> failwith "bad coder1"
              end)
        else
          State (fun x -> [(new_end_ref,x)])
      in
      r1_end_ref := new_inner_end;
      r2_end_ref := new_inner_end;
      (ref new_start,new_end_ref)
  | RegExStar (inner_r) ->
      let (inner_start_ref,inner_end_ref) = regex_to_dfa c mcs inner_r inside_userdef in
      let new_end_ref = ref QAccept in
      let new_inner_end_fun =
        if not inside_userdef then
          (fun (s,er,rc,is,so) ->
            begin match (is,rc) with
            | (i::it,h::t) -> (inner_start_ref, (s,er,rc,(i+1)::it,so))::
              [(new_end_ref,(s,h s er,t,it,so))]
            | _ -> failwith "bad coder2"
            end)
        else
          (fun x -> [(inner_start_ref, x);(new_end_ref, x)])
      in
      inner_end_ref := State new_inner_end_fun;
      let new_start_fun = 
        if not inside_userdef then
          (fun (s,er,rc,is,so) ->
            begin match er with
            | ERegExStar (er',il) ->
              let rc_add_star =
                (fun _ er'' ->
                  ERegExStar (er'',is::il)) in
                [(inner_end_ref,(s,er',rc_add_star::rc,-1::is,so))]
            | _ -> failwith (s ^ " " ^ (Pp.pp_exampled_regex er))
            end)
        else
          (fun x -> [(inner_end_ref,x)])
      in
      let new_start = State new_start_fun in
      (ref new_start, new_end_ref)
  | RegExUserDefined t ->
      let rex = RegexContext.lookup_exn c t in
      let (inner_start_ref,inner_end_ref) = regex_to_dfa c mcs rex true in
      let new_end_ref = ref QAccept in
      let new_start_fun =
        if not inside_userdef then
          (fun ((s,er,rc,is,so):data) ->
            [(inner_start_ref,(s,er,
              (fun s' er' ->
                begin match er' with
                | ERegExUserDefined (t,l,il) -> ERegExUserDefined
                    (t,(String.chop_suffix_exn ~suffix:s' s)::l,is::il)
                | _ -> failwith (Pp.pp_exampled_regex er')
                end)::rc,is,Some s))]
          )
        else
          (fun x -> [(inner_start_ref,x)])
      in
      let new_start_state = State new_start_fun in
      let new_inner_end_fun =
        (fun (s,er,rc,is,so) ->
          if not inside_userdef then
            begin match rc with
            | [] -> failwith "bad coding2"
            | h::t -> [(new_end_ref,(s,h s er,t,is,so))]
            end
          else
            [(new_end_ref,(s,er,rc,is,so))]) in
      inner_end_ref := (State new_inner_end_fun);
      (ref new_start_state,new_end_ref)
  | RegExMappedUserDefined t ->
      begin match List.Assoc.find mcs t with
      | Some rextemp ->
          let rex = dnf_regex_to_regex rextemp in
          let (inner_start_ref,inner_end_ref) = regex_to_dfa c mcs rex true in
          let new_end_ref = ref QAccept in
          let new_start_fun =
            if not inside_userdef then
              (fun ((s,er,rc,is,so):data) ->
                [(inner_start_ref,(s,er,
                  (fun s' er' ->
                    begin match er' with
                    | ERegExMappedUserDefined (t,l,il) -> ERegExMappedUserDefined
                        (t,(String.chop_suffix_exn ~suffix:s' s)::l,is::il)
                    | _ -> failwith (Pp.pp_exampled_regex er')
                    end)::rc,is,Some s))]
              )
            else
              (fun x -> [(inner_start_ref,x)])
          in
          let new_start_state = State new_start_fun in
          let new_inner_end_fun =
            (fun (s,er,rc,is,so) ->
              if not inside_userdef then
                begin match rc with
                | [] -> failwith "bad coding2"
                | h::t -> [(new_end_ref,(s,h s er,t,is,so))]
                end
              else
                [(new_end_ref,(s,er,rc,is,so))]) in
          inner_end_ref := (State new_inner_end_fun);
          (ref new_start_state,new_end_ref)
      | None -> failwith ("not in contextz: " ^ (string_of_int (List.length mcs)))
      end
  end

let rec eval_dfa (st:state) ((s,er,recombiners,is,so):data) :
  exampled_regex option =
  begin match st with
  | State (f) ->
        let state_string_list = f (s,er,recombiners,is,so) in
        List.fold_left
        ~f:(fun acc (st',(s',er',rc,is,so)) ->
          begin match acc with
          | None ->  eval_dfa (!st') (s',er',rc,is,so)
          | _ -> acc
          end)
        ~init:None
        state_string_list
  | QAccept ->
      if s = "" then
        Some er
      else
        None
  end

let rec fast_eval (c:RegexContext.t) (mcs:mapsbetweencontextside) (r:regex) (s:string) : bool =
  let (dfa_start,_) = regex_to_dfa c mcs r false in
  begin match eval_dfa !dfa_start (s,(to_empty_exampled_regex r),[],[0],None) with
  | None -> false
  | Some er -> true
  end

let rec regex_to_exampled_regex (rc:RegexContext.t) (mcs:mapsbetweencontextside) (r:regex) (es:string list)
                                 : exampled_regex option =
  let (dfa_start,_) = regex_to_dfa rc mcs r false in
  let start_state = !dfa_start in
  List.foldi
  ~f:(fun i er e ->
    begin match er with
    | None -> None
    | Some er' -> eval_dfa start_state (e,er',[],[i],None)
    end)
  ~init:(Some (to_empty_exampled_regex r))
  es

let rec clean_exampledness_atom (choices:int list list) (a:exampled_atom)  : exampled_atom =
  begin match a with
  | EAUserDefined (s,el,cs) ->
      let udef_choice_zip = List.zip_exn el cs in
      let actual_choices =
        List.filter
          ~f:(fun (el,c) -> List.mem choices c )
          udef_choice_zip
        in
        let (strs,cs) = List.unzip actual_choices in
      EAUserDefined (s,strs,cs)
  | EAMappedUserDefined (s,el,cs) ->
      let udef_choice_zip = List.zip_exn el cs in
      let actual_choices =
        List.filter
          ~f:(fun (el,c) -> List.mem choices c )
          udef_choice_zip
        in
        let (strs,cs) = List.unzip actual_choices in
      EAMappedUserDefined (s,strs,cs)
  | EAStar (r,cs) ->
      
  let actual_choices =
    List.filter
      ~f:(fun ch -> List.mem choices ch)
      cs
        in
      
      EAStar (clean_exampledness_dnf_regex actual_choices r, actual_choices)
  end
and clean_exampledness_clause (above_choices:int list list)
((atoms,strings,current_choices):exampled_clause) : exampled_clause =


  let actual_choices =
    List.filter
      ~f:(fun ch -> List.mem above_choices ch)
      current_choices
        in

  (
    List.map ~f:(clean_exampledness_atom actual_choices) atoms,
    strings,
    actual_choices)


and clean_exampledness_dnf_regex (above_choices:int list list)
((clauses,current_choices):exampled_dnf_regex)  : exampled_dnf_regex =

  let rec is_suplist (lowerc:int list) (upperc:int list) : bool =
    begin match (lowerc,upperc) with
    | (h1::t1,h2::t2) ->
        if h1 = h2 then
          is_suplist t1 t2
        else
          false
    | (_,[]) -> true
    | _ -> false
    end
  in
  let rec contains_sublist (viable_choices:int list list) (lowerc:int list) 
    : bool =
      List.exists ~f:(is_suplist (List.rev lowerc)) (List.map ~f:List.rev
      viable_choices)
  in

  let viable_choices = List.filter ~f:(contains_sublist above_choices)
  current_choices in



  (List.map ~f:(clean_exampledness_clause viable_choices) clauses,viable_choices)



let rec concat_exampled_dnf_regexs ((r1,c1):exampled_dnf_regex)
                                   ((r2,c2):exampled_dnf_regex)
                                   : exampled_clause list =
  cartesian_map
    (fun (a1s,s1s,c1s) (a2s,s2s,c2s) ->
      let choices_taken = intersect_lose_order_no_dupes
        (dictionary_order (int_comparer_to_comparer compare))
        c1s
        c2s in
      (List.map ~f:(clean_exampledness_atom choices_taken)(a1s@a2s),
      weld_lists (^) s1s s2s,
      choices_taken))
    r1
    r2 (*TODO make test that checks that won't get the information propagated*)

let rec or_exampled_dnf_regexs ((r1,c1):exampled_dnf_regex)
                               ((r2,c2):exampled_dnf_regex)
                               : exampled_clause list =
  r1@r2

let rec exampled_atom_to_exampled_dnf_regex
          (a:exampled_atom)
          (ill:int list list)
          : exampled_dnf_regex =
  ([([a],["";""],ill)],ill)

let rec exampled_regex_to_exampled_dnf_regex (r:exampled_regex) :
  exampled_dnf_regex =
  begin match r with
  | ERegExEmpty -> ([],[])
  | ERegExBase (c, ill) -> ([([],[c],ill)],ill)
  | ERegExConcat (r1,r2,ill) ->
      (concat_exampled_dnf_regexs
        (exampled_regex_to_exampled_dnf_regex r1)
        (exampled_regex_to_exampled_dnf_regex r2),ill)
  | ERegExOr (r1,r2,ill) ->
      (or_exampled_dnf_regexs
        (exampled_regex_to_exampled_dnf_regex r1)
        (exampled_regex_to_exampled_dnf_regex r2),ill)
  | ERegExStar (r',ill) ->
      exampled_atom_to_exampled_dnf_regex
        (EAStar (exampled_regex_to_exampled_dnf_regex r',ill))
        ill
  | ERegExUserDefined (s,ss,ill) ->
      exampled_atom_to_exampled_dnf_regex
        (EAUserDefined (s,ss,ill))
        ill
  | ERegExMappedUserDefined (s,ss,ill) ->
      exampled_atom_to_exampled_dnf_regex
        (EAMappedUserDefined (s,ss,ill))
        ill
  end

let regex_to_exampled_dnf_regex (c:RegexContext.t)
                                (mcs:mapsbetweencontextside)
                                (r:regex)
                                (es:string list)
                                : exampled_dnf_regex option =
  let er_option = regex_to_exampled_regex c mcs r es in
  Option.map ~f:exampled_regex_to_exampled_dnf_regex er_option
