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

let rec eval_lens (l:lens) (s:string) : string option =
  begin match l with
  | ConstLens (a,b) -> if s = a then Some b else None
  | ConcatLens (l1,l2) -> failwith "ahh"
  | _ -> failwith "ahh"
  end
