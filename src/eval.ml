open Lang
open Regexcontext
open Core.Std
open Util
open Lens

(* we assume the regex satisfies constraints of unique parsing *)
let rec eval_regex (c:RegexContext.t) (r:regex) (s:string) : bool =
  begin match r with
  | RegExBase s' -> s' = s
  | RegExConcat (r1,r2) ->
      let strlen = String.length s in
      (List.fold_left
        ~f:(fun acc x -> acc
            || ((eval_regex c r1 (String.sub s 0 x))
                && (eval_regex c r2 (String.sub s x (strlen-x)))))
        ~init:false
        (range 0 strlen)
      )
  | RegExOr (r1,r2) ->
      (eval_regex c r1 s) || (eval_regex c r2 s)
  | RegExStar r' ->
      if (s = "") then true
      else
        let strlen = String.length s in
        (List.fold_left
          ~f:(fun acc x -> acc
              || ((eval_regex c r' (String.sub s 0 x))
                  && (eval_regex c r (String.sub s x (strlen-x)))))
          ~init:false
          (range 1 strlen)
        )
  | RegExUserDefined t ->
      let rex = RegexContext.lookup_exn c t in
      eval_regex c rex s
  end

let rec retrieve_regex_or_choice (c:RegexContext.t)
  (r1:regex) (r2:regex) (s:string) : bool option =
    begin match (eval_regex c r1 s, eval_regex c r2 s) with
    | (true,false) -> Some false
    | (false,true) -> Some true
    | (false,false) -> None
    | (true,true) -> failwith "not disjoint union"
    end

let rec retrieve_regex_concat_split (c:RegexContext.t)
  (r1:regex) (r2:regex) (s:string) : (string * string) option =
    let strlen = String.length s in
    let splits = List.filter_map
      ~f:(fun i ->
          let lhs = String.sub s 0 i in
          let rhs = String.sub s i (strlen-i) in
          if ((eval_regex c r1 lhs)
              && (eval_regex c r2 rhs)) then
            Some (lhs,rhs)
          else
            None)
      (range 0 strlen)
    in
    begin match splits with
    | [] -> None
    | [s] -> Some s
    | _ -> failwith "unambiguous concat"
    end

let retrieve_regex_star_splits (c:RegexContext.t)
  (r:regex) (s:string) : (string list) option =
    let rec retrieve_regex_star_splits_internal (s:string)
      : (string list) option =
        let strlen = String.length s in
        let splits = List.filter_map
          ~f:(fun i ->
              let lhs = String.sub s 0 i in
              let rhs = String.sub s i (strlen-i) in
              if (not (eval_regex c r lhs)) then
                None
              else
                if (rhs = "") then
                  Some [lhs]
                else
                  begin match retrieve_regex_star_splits_internal rhs with
                  | None -> (print_endline "hi");None
                  | Some splits -> Some (lhs::splits)
                  end
                )
          (range 0 strlen)
        in
        begin match splits with
        | [] -> None
        | [s] -> Some s
        | _ -> failwith "unambiguous concat"
        end
    in
    if (eval_regex c r "") then failwith "shouldn't match empty"
    else
      retrieve_regex_star_splits_internal s

let rec eval_atom (c:RegexContext.t) (a:atom) (s:string) : bool =
  begin match a with
    | AUserDefined v ->
      let rex = RegexContext.lookup_exn c v in
      eval_regex c rex s
  | AStar dnf_regex ->
      if (s = "") then true
      else
        let strlen = String.length s in
        (List.fold_left
          ~f:(fun acc x -> acc
              || ((eval_dnf_regex c dnf_regex (String.sub s 0 x))
                  && (eval_atom c a (String.sub s x (strlen-x)))))
          ~init:false
          (range 1 strlen)
        )
  end

and eval_clause (c:RegexContext.t) (cl:clause) (s:string) : bool =
  let rec eval_clause_string ((atoms,strings):clause) (s:string) : bool =
    begin match strings with
    | [] -> failwith "invalid clause"
    | h::t -> begin match String.chop_prefix s ~prefix:h with
              | None -> false
              | Some s' -> eval_clause_atom (atoms,t) s'
              end
    end
  and eval_clause_atom ((atoms,strings):clause) (s:string) : bool =
    begin match atoms with
    | [] -> s = ""
    | h::t -> List.exists
      ~f:(fun pos ->
        eval_atom c h (String.prefix s pos)
        && eval_clause_string (t,strings) (String.drop_prefix s pos))
      (range 0 (String.length s))
    end
  in
  eval_clause_string cl s

and eval_dnf_regex (c:RegexContext.t) (clauses:dnf_regex) (s:string) : bool =
  List.exists
    ~f:(fun clause -> eval_clause c clause s)
    clauses


let retrieve_atom_splits (c:RegexContext.t) (cl:clause)
                                (s:string) : (string list) option =
  let rec retrieve_splits_string ((atoms,strings):clause)
                                (s:string) : (string list) option =
    begin match strings with
    | [] -> failwith "invalid clause"
    | h::t -> begin match String.chop_prefix s ~prefix:h with
              | None -> None
              | Some s' -> retrieve_splits_atom (atoms,t) s'
              end
    end
  and retrieve_splits_atom ((atoms,strings):clause)
                                (s:string) : (string list) option =
    begin match atoms with
    | [] -> if s = "" then Some [] else None
    | h::t -> List.fold_left
      ~f:(fun acc pos ->
        begin match acc with
        | None ->
            if (eval_atom c h (String.prefix s pos)) then
              begin match retrieve_splits_string (t,strings) (String.drop_prefix s
              pos) with
              | None -> None
              | Some x -> (Some ((String.prefix s pos)::x))
              end
            else
              None
        | Some _ -> acc
        end)
      ~init:None
      (range 0 (String.length s))
    end
  in
  retrieve_splits_string cl s

let retrieve_dnf_clause_choices (c:RegexContext.t) (clauses:clause list)
                                (s:string) : int option =
  List.foldi
  ~f:(fun i acc x ->
    let accept = eval_clause c x s in
    begin match (acc,accept) with
    | (None,true) -> Some i
    | (None,false) -> None
    | (Some _, false) -> acc
    | (Some _, true) -> failwith "not disjoint union"
    end)
  ~init:None
  clauses

let retrieve_dnf_star_splits (c:RegexContext.t)
  (r:dnf_regex) (s:string) : (string list) option =
    let rec retrieve_regex_star_splits_internal (s:string)
      : (string list) option =
        let strlen = String.length s in
        let splits = List.filter_map
          ~f:(fun i ->
              let lhs = String.sub s 0 i in
              let rhs = String.sub s i (strlen-i) in
              if (not (eval_dnf_regex c r lhs)) then
                None
              else
                if (rhs = "") then
                  Some [lhs]
                else
                  begin match retrieve_regex_star_splits_internal rhs with
                  | None -> (print_endline "hi");None
                  | Some splits -> Some (lhs::splits)
                  end
                )
          (range 0 strlen)
        in
        begin match splits with
        | [] -> None
        | [s] -> Some s
        | _ -> failwith "unambiguous concat"
        end
    in
    if (eval_dnf_regex c r "") then failwith "shouldn't match empty"
    else if s = "" then
      Some []
    else
      retrieve_regex_star_splits_internal s

let rec eval_lens (l:lens) (s:string) : string option =
  begin match l with
  | ConstLens (a,b) -> if s = a then Some b else None
  | ConcatLens (l1,l2) -> failwith "ahh"
  | _ -> failwith "ahh"
  end

(*let rec count_maximum_expansions_regex (c:context) (r:regex) (es:string list)
                                      : int option =
  begin match r with
  | RegExBase s ->
      if List.for_all ~f:(fun e -> e = s) es then
        None 
      else
        Some 0
  | RegExConcat (r1,r2) ->
      let example_splits = List.map
        ~f:(fun e -> retrieve_regex_concat_split c r1 r2 e)
        es in
      begin match distribute_option example_splits with
      | None -> None
      | Some splits ->
          let (left_side,right_side) = List.unzip splits in
          let max_left_option = count_maximum_expansions_regex c r1 left_side in
          let max_right_option = count_maximum_expansions_regex c r2 right_side in
          begin match (max_left_option,max_right_option) with
          | (Some c1, Some c2) -> Some (c1+c2)
          | _ -> None
          end
      end
  | _ -> failwith "bad idea"
  end*)

(* ASSUMES EXAMPLES MATCH REGEXS *)
let to_exampled_dnf_regex (c:RegexContext.t) (r:dnf_regex) (exs_side:string list)
                        : exampled_dnf_regex option =
  let rec to_exampled_atom_bare (a:atom) : exampled_atom =
    begin match a with
    | AUserDefined s -> EAUserDefined (s,[],[])
    | AStar r -> EAStar (to_exampled_dnf_regex_bare r,[])
    end
  and to_exampled_clause_bare ((atoms,strings):clause) : exampled_clause =
    (List.map ~f:(fun a -> to_exampled_atom_bare a) atoms,strings,[])
  and to_exampled_dnf_regex_bare (r:dnf_regex) : exampled_dnf_regex =
    (List.map ~f:(fun c -> to_exampled_clause_bare c) r,[])
  in

  let rec add_atom_example (a:exampled_atom) (s:string)
                        : exampled_atom option =
    let rec add_atom_example_internal (a:exampled_atom) (s:string) (i:int)
                          : exampled_atom option =
      begin match a with
        | EAUserDefined (v,exs,ill) ->
          let rex = RegexContext.lookup_exn c v in
              if (eval_regex c rex s) then
                Some (EAUserDefined (v,s::exs,ill))
              else
                None
      | EAStar (dnf,ill) ->
          if (s = "") then Some a
          else
            let strlen = String.length s in
            List.fold_left
              ~f:(fun acc x ->
                begin match acc with
                | None ->
                    begin match add_dnf_regex_example dnf (String.sub s 0 x) i with
                    | None -> None
                    | Some dnf' -> add_atom_example_internal
                                    (EAStar (dnf',ill))
                                    (String.sub s x (strlen-x))
                                    (i+1)
                    end
                | Some _ -> acc
                end)
              ~init:None
              (range 1 strlen)
      end
    in
  add_atom_example_internal a s 0

  and add_clause_example ((atoms,strings,choices):exampled_clause) (s:string) (i:int)
                        : (exampled_clause option) =
    let rec retrieve_splits_string (atoms:exampled_atom list)
                        (strings:string list)
                        (s:string) : exampled_atom list option =
      begin match strings with
      | [] -> failwith "invalid clause"
      | h::t -> begin match String.chop_prefix s ~prefix:h with
                | None -> None
                | Some s' -> retrieve_splits_atom atoms t s'
                end
      end
    and retrieve_splits_atom (atoms:exampled_atom list)
                          (strings:string list)
                          (s:string) : exampled_atom list option =
      begin match atoms with
      | [] -> if s = "" then Some [] else None
      | h::t -> List.fold_left
        ~f:(fun acc pos ->
          begin match acc with
          | None ->
              begin match add_atom_example h (String.prefix s pos) with
              | None -> None
              | Some a ->
                begin match retrieve_splits_string t strings
                (String.drop_prefix s pos) with
                | None -> None
                | Some x -> (Some (a::x))
                end
              end
          | Some _ -> acc
          end)
        ~init:None
        (range 0 (String.length s))
      end
    in

    begin match retrieve_splits_string atoms strings s with
    | None -> None
    | Some atoms' -> Some (atoms',strings,[i]::choices)
    end

  and add_dnf_regex_example ((r,ill):exampled_dnf_regex) (s:string) (i:int)
                        : exampled_dnf_regex option =
    let (r',found) = List.fold_right
      ~f:(fun c (acc,found) ->
        if found then
          (c::acc,true)
        else
          begin match add_clause_example c s i with
          | None -> (c::acc,false)
          | Some c' -> (c'::acc,true)
          end)
      ~init:([],false)
      r in
    if found then Some (r',ill) else None
  in

  List.foldi
    ~f:(fun i acc s ->
      begin match acc with
      | None -> None
      | Some acc' ->
          begin match add_dnf_regex_example acc' s i with
          | Some x -> Some x
          | None -> failwith s
          end
      end)
    ~init: (Some (to_exampled_dnf_regex_bare r))
    exs_side
