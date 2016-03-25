open Lang
open Core.Std
open Util
open Lens

(* we assume the regex satisfies constraints of unique parsing *)
let rec eval_regex (c:context) (r:regex) (s:string) : bool =
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
      begin match List.Assoc.find c t with
      | Some rex -> eval_regex c rex s
      | None -> failwith "not in the context"
      end
  end

let rec retrieve_regex_or_choice (c:context)
  (r1:regex) (r2:regex) (s:string) : bool option =
    begin match (eval_regex c r1 s, eval_regex c r2 s) with
    | (true,false) -> Some false
    | (false,true) -> Some true
    | (false,false) -> None
    | (true,true) -> failwith "not disjoint union"
    end

let rec retrieve_regex_concat_split (c:context)
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

let retrieve_regex_star_splits (c:context)
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

let rec eval_atom (c:context) (a:atom) (s:string) : bool =
  begin match a with
  | AUserDefined v ->
      begin match List.Assoc.find c v with
      | Some rex -> eval_regex c rex s
      | None -> failwith "not in the context"
      end
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

and eval_clause (c:context) (cl:clause) (s:string) : bool =
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

and eval_dnf_regex (c:context) (clauses:dnf_regex) (s:string) : bool =
  List.exists
    ~f:(fun clause -> eval_clause c clause s)
    clauses


let retrieve_atom_splits (c:context) (cl:clause)
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

let retrieve_dnf_clause_choices (c:context) (clauses:clause list)
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

let retrieve_dnf_star_splits (c:context)
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
    else
      retrieve_regex_star_splits_internal s

let rec eval_lens (l:lens) (s:string) : string option =
  begin match l with
  | ConstLens (a,b) -> if s = a then Some b else None
  | ConcatLens (l1,l2) -> failwith "ahh"
  | _ -> failwith "ahh"
  end
