open Core.Std
open Printf
open Util
open Fn
open Regex

(**** Language {{{ *****)

exception Internal_error of string
let internal_error f s = raise @@ Internal_error (sprintf "(%s) %s" f s)

type id = string


type lens =
  | LensConst of string * string
  | LensConcat of lens * lens
  | LensSwap of lens * lens
  | LensUnion of lens * lens
  | LensCompose of lens * lens
  | LensIterate of lens
  | LensIdentity of regex
  | LensInverse of lens
  | LensVariable of id






type examples = (string * string) list

type specification = (string * regex * regex * (string * string) list)

type declaration =
  | DeclUserdefCreation of (string * regex * bool)
  | DeclTestString of (regex * string)
  | DeclSynthesizeProgram of specification

type program = declaration list

type synth_problems = (string * regex * bool) list * (specification list) 








type atom =
  | AMappedUserDefined of int
  | AUserDefined of string
  | AStar of dnf_regex

and clause = atom list * string list

and dnf_regex = clause list

let empty_dnf_string : dnf_regex = [([],[""])]







let rec concat_dnf_regexs (r1:dnf_regex) (r2:dnf_regex) : dnf_regex =
  cartesian_map
    (fun (a1s,s1s) (a2s,s2s) -> (a1s@a2s,weld_lists (^) s1s s2s))
    r1
    r2

let rec or_dnf_regexs (r1:dnf_regex) (r2:dnf_regex) : dnf_regex =
  r1 @ r2

let rec concat_clause_dnf_rx (cl:clause) (r:dnf_regex) : dnf_regex =
  concat_dnf_regexs [cl] r

let rec concat_dnf_rx_clause (r:dnf_regex) (cl:clause) : dnf_regex =
  concat_dnf_regexs r [cl]

let rec exponentiate_dnf (r:dnf_regex) (n:int) : dnf_regex =
  if n < 0 then
    failwith "invalid exponential"
  else if n = 0 then
    empty_dnf_string
  else
    concat_dnf_regexs (exponentiate_dnf r (n-1)) r

let rec quotiented_star_dnf (r:dnf_regex) (n:int) : dnf_regex =
  if n < 1 then
    failwith "invalid modulation"
  else if n = 1 then
    empty_dnf_string
  else
    or_dnf_regexs (quotiented_star_dnf r (n-1)) (exponentiate_dnf r (n-1))

let rec singleton_atom (a:atom) : dnf_regex =
  [([a],["";""])]

let rec to_dnf_regex (r:regex) : dnf_regex =
  let rec atom_to_dnf_regex (a:atom) : dnf_regex =
    [([a],["";""])]
  in
  begin match r with
  | RegExMapped t -> atom_to_dnf_regex (AMappedUserDefined t)
  | RegExEmpty -> []
  | RegExBase c -> [([],[c])]
  | RegExConcat (r1,r2) ->
      cartesian_map
        (fun (a1s,s1s) (a2s,s2s) -> (a1s@a2s,weld_lists (^) s1s s2s))
        (to_dnf_regex r1)
        (to_dnf_regex r2)
  | RegExOr (r1, r2) -> (to_dnf_regex r1) @ (to_dnf_regex r2)
  | RegExStar (r') -> atom_to_dnf_regex (AStar (to_dnf_regex r'))
  | RegExVariable s -> atom_to_dnf_regex (AUserDefined s)
  end


let rec compare_atoms (a1:atom) (a2:atom) : comparison =
  begin match (a1,a2) with
  | (AMappedUserDefined s1, AMappedUserDefined s2) ->
      int_to_comparison (compare s1 s2)
  | (AMappedUserDefined _, _) -> LT
  | (_, AMappedUserDefined _) -> GT
  | (AUserDefined s1, AUserDefined s2) -> int_to_comparison (compare s1 s2)
  | (AUserDefined  _, AStar         _) -> LT
  | (AStar         _, AUserDefined  _) -> GT
  | (AStar        r1, AStar        r2) -> compare_dnf_regexs r1 r2
  end

and compare_clauses ((atoms1,strings1):clause) ((atoms2,strings2):clause) : comparison =
  ordered_partition_order compare_atoms atoms1 atoms2

and compare_dnf_regexs (r1:dnf_regex) (r2:dnf_regex) : comparison =
  ordered_partition_order compare_clauses r1 r2


type exampled_regex =
  | ERegExEmpty
  | ERegExBase of string * (int list list)
  | ERegExConcat of exampled_regex * exampled_regex * (int list list)
  | ERegExOr of exampled_regex  * exampled_regex * (int list list)
  | ERegExStar of exampled_regex * (int list list)
  | ERegExVariable of string * string list * (int list list)
  | ERegExMapped of int * string list * (int list list)

let extract_iterations_consumed (er:exampled_regex) : int list list =
  begin match er with
    | ERegExEmpty -> []
    | ERegExBase (_,ill) -> ill
    | ERegExConcat (_,_,ill) -> ill
    | ERegExOr (_,_,ill) -> ill
    | ERegExStar (_,ill) -> ill
    | ERegExVariable (_,_,ill) -> ill
    | ERegExMapped (_,_,ill) -> ill
  end

let rec took_regex (er:exampled_regex)
    (iteration:int list) : bool =
  let ill = extract_iterations_consumed er in
  List.mem ill iteration

let rec extract_string (er:exampled_regex) (iteration:int list)
  : string =
  begin match er with
    | ERegExEmpty -> failwith "no string"
    | ERegExBase (s,_) -> s
    | ERegExConcat (er1,er2,_) ->
      (extract_string er1 iteration) ^
      (extract_string er2 iteration)
    | ERegExOr (er1,er2,_) ->
      if took_regex er1 iteration then
        extract_string er1 iteration
      else
        extract_string er2 iteration
    | ERegExStar (er',ill) ->
        let valid_iterations =
          List.rev
            (List.filter
              ~f:(fun it -> List.tl_exn it = iteration)
              (extract_iterations_consumed er')) in
        String.concat
          (List.map
            ~f:(extract_string er')
            valid_iterations)
    | ERegExVariable (_,sl,ill) ->
        let dat_opt = List.findi
          ~f:(fun i il -> il = iteration)
          ill in
        begin match dat_opt with
        | None -> failwith "im horrible"
        | Some (i,_) ->
            List.nth_exn sl i
        end
    | ERegExMapped (_,sl,ill) ->
        let dat_opt = List.findi
          ~f:(fun i il -> il = iteration)
          ill in
        begin match dat_opt with
        | None -> failwith "im horrible"
        | Some (i,_) ->
            List.nth_exn sl i
        end
  end


let rec extract_example_list (er:exampled_regex) : int list list =
  begin match er with
  | ERegExEmpty -> []
  | ERegExBase (_,ill) -> ill
  | ERegExConcat (_,_,ill) -> ill
  | ERegExOr (_,_,ill) -> ill
  | ERegExStar (_,ill) -> ill
  | ERegExVariable (_,_,ill) -> ill
  | ERegExMapped (_,_,ill) -> ill
  end



(* mbc *)
type mapsbetweencontext = ((int * (dnf_regex * dnf_regex)) list * int)

let emptymapsbetweencontext = ([],0)

let add_to_context  ((cl,n):mapsbetweencontext)
                    (r1:dnf_regex)
                    (r2:dnf_regex)
                    : mapsbetweencontext * int =
  begin match assoc_value_mem (r1,r2) cl with
  | Some k -> ((cl,n),k)
  | None -> (((n,(r1,r2))::cl,n+1),n)
  end

type mapsbetweencontextside = (int * dnf_regex) list

let get_left_side ((c,_):mapsbetweencontext) =
  List.map ~f:(fun (i,(l,_)) -> (i,l)) c

let get_right_side ((c,_):mapsbetweencontext) =
  List.map ~f:(fun (i,(l,_)) -> (i,l)) c

let rec atom_to_regex (a:atom) : regex =
  begin match a with
  | AMappedUserDefined t -> RegExMapped t
  | AUserDefined t -> RegExVariable t
  | AStar dr -> RegExStar (dnf_regex_to_regex dr)
  end

and clause_to_regex ((atoms,strings):clause) : regex =
  let atoms_regex_list = List.map
    ~f:(fun a -> atom_to_regex a)
    atoms
  in
  let (hstr,tstr) = split_by_first_exn strings in
  let aslist = List.zip_exn atoms_regex_list tstr in
  List.fold_left
    ~f:(fun acc (a,s) ->
      RegExConcat(acc,RegExConcat(a,RegExBase s)))
    ~init:(RegExBase hstr)
    aslist

and dnf_regex_to_regex (r:dnf_regex) : regex =
  let sequence_regex_list = List.map
    ~f:(fun c -> clause_to_regex c)
    r
  in
  List.fold_left
  ~f:(fun acc sqr ->
    RegExOr (acc,sqr))
  ~init:RegExEmpty
  sequence_regex_list


      

type queue_element =
  | QERegexCombo of regex * regex * int * mapsbetweencontext
  | QEGenerator of (unit -> ((queue_element * float) list))


let rec clause_to_kvp ((atoms,strings):clause)
      : ((atom * string) option * clause) =
  begin match (atoms,strings) with
  | (ah::at,sh::st) -> (Some (ah,sh), (at,st))
  | ([],[s]) -> (None,([],[s]))
  | _ -> failwith "malformed clause"
  end

let rec tl_regex_to_regex (tl:(((atom * string) option) list,
string) tagged_list_tree) : regex =
  begin match tl with
  | Leaf s -> RegExBase s
  | Node (asl,tll) ->
      let left_side = List.fold_left
        ~f:(fun acc aso ->
          begin match aso with
          | None -> RegExBase ""
          | Some (a,s) ->
              RegExOr(acc,RegExConcat (RegExBase s,smart_atom_to_regex a))
          end)
        ~init:RegExEmpty
        asl
      in
      let right_side = tll_regex_to_regex tll in
      RegExConcat(left_side,right_side)
  end

and tll_regex_to_regex (tll:(((atom * string) option) list,
string) tagged_list_tree list) : regex =
        List.fold_left
          ~f:(fun acc l -> RegExOr(acc, tl_regex_to_regex l))
          ~init:RegExEmpty
          tll

and smart_dnf_regex_to_regex (r:dnf_regex) : regex =
  let tltl = dnf_regex_to_tagged_list_tree_list r in
  let tltl_grouped = List.map ~f:tagged_list_tree_keygrouped tltl in
  let real_grouped_tltl = handle_noded_tltl tltl_grouped in
  tll_regex_to_regex real_grouped_tltl

and dnf_regex_to_tagged_list_tree_list (r:dnf_regex) : ((atom * string) option,
string) tagged_list_tree list =
  let kvp_list = List.map ~f:clause_to_kvp r in
  let keys = List.dedup (List.map ~f:fst kvp_list) in
  let test = List.fold_left
    ~f:(fun acc (k,v) ->
        insert_into_correct_list (k,v) acc)
    ~init:(List.map ~f:(fun key -> (key,[])) keys)
    kvp_list
  in
  List.map
    ~f:(fun (k,vl) ->
      begin match k with
      | None -> Node (k,
          List.map
            ~f:(fun x ->
              begin match x with
              | ([],[s]) -> Leaf s
              | _ -> failwith "bad"
              end)
            vl)
      | Some _ -> Node (k, dnf_regex_to_tagged_list_tree_list vl)
      end)
    test

and smart_atom_to_regex (a:atom) : regex =
  begin match a with
  | AMappedUserDefined t -> RegExMapped t
  | AUserDefined t -> RegExVariable t
  | AStar dr -> RegExStar (smart_dnf_regex_to_regex dr)
  end

let rec clean_regex (r:regex) : regex =
  begin match r with
  | RegExConcat(x,y) ->
    let x = clean_regex x in
    let y = clean_regex y in
    begin match (x,y) with
    | (RegExBase "",_) -> y
    | (_,RegExBase "") -> x
    | (RegExEmpty,_) -> RegExEmpty
    | (_,RegExEmpty) -> RegExEmpty
    | _ -> RegExConcat(x,y)
    end
  | RegExOr(x,y) ->
    let x = clean_regex x in
    let y = clean_regex y in
    begin match (x,y) with
    | (RegExEmpty,_) -> y
    | (_,RegExEmpty) -> x
    | _ -> RegExOr(x,y)
    end
  | RegExStar(x) ->
    let x = clean_regex x in
    begin match x with
    | RegExEmpty -> RegExBase ""
    | _ -> x
    end
  | _ -> r
  end



(***** }}} *****)
