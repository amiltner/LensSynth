open Core.Std
open Util
open String_utilities
open Lang

(***** The main RegexContext module {{{ *****)

module type RegexContext_Sig = sig
    type t

    val empty                    : t
    val lookup_everything        : t -> id -> (regex * bool) option
    val lookup                   : t -> id -> regex option
    val lookup_exn               : t -> id -> regex
    val insert_exn               : ?quotient:bool -> t -> id -> regex -> bool -> t
    val insert_list_exn          : t -> (id * regex * bool) list -> t
    val create_from_list_exn     : (id * regex * bool) list -> t
    val lookup_for_expansion_exn : t -> id -> regex option
    val autogen_id               : t -> regex -> id
    val autogen_fresh_id         : t -> id
    val merge_contexts_exn       : t -> t -> t
    val compare                  : t -> t -> comparison
    val to_string                : t -> string
    val hash                     : t -> int
end

module RegexContext : RegexContext_Sig = struct
  module D = Dict.Make(
    struct
      type key = id
      type value = regex * bool
      let compare_key = comparison_compare
      let compare_value = pair_compare regex_compare comparison_compare
      let key_to_string = ident
      let value_to_string = string_of_pair regex_to_string string_of_bool
    end)
    type t = D.dict

    let empty = D.empty

    let lookup_everything (rc:t) (name:id) : (regex*bool) option =
      D.lookup rc name

    let lookup (rc:t) (name:id) : regex option =
      Option.map ~f:fst (lookup_everything rc name)

    let lookup_exn (rc:t) (name:id) : regex =
      begin match lookup rc name with
        | None -> failwith "lookup_exn: key not found"
        | Some v -> v
      end

    let insert_exn ?quotient:(quotient=false) (rc:t) (name:id) (r:regex) (is_abstract:bool) : t =
      begin match lookup_everything rc name with
        | None -> D.insert rc name (r,is_abstract)
        | Some ra ->
					if not quotient then
            if ra = (r,is_abstract) then
              rc
            else
              failwith (name ^ " already exists in the context")
					else
						D.insert rc name (r,is_abstract)
      end

    let insert_list_exn (rc:t) (nral:(id * regex * bool) list) : t =
      List.fold_left
        ~f:(fun acc (name,r,b) -> insert_exn acc name r b)
        ~init:rc
        nral

    let create_from_list_exn (nral:(id * regex * bool) list) : t =
      insert_list_exn empty nral

    let autogen_id (rc:t) (r:regex) : id =
      let base = regex_to_string r in
      let rec fresh n =
        let x = Printf.sprintf "%s%d" base n in
        begin match D.lookup rc x with
          | Some (r',false) ->
            if r = r' then
              x
            else
              fresh (n+1)
          | Some _ -> fresh (n+1)
          | _ -> x
        end
      in
      fresh 1

    let autogen_fresh_id (rc:t) : id =
      let base = "r" in
      let rec fresh n =
        let x = Printf.sprintf "%s%d" base n in
        begin match D.lookup rc x with
          | Some _ -> fresh (n+1)
          | _ -> x
        end
      in
      fresh 1

    let lookup_for_expansion_exn (rc:t) (name:id) : regex option =
      begin match lookup_everything rc name with
        | None -> failwith ("bad regex name: " ^ name)
        | Some (r,abs) ->
          if abs then
            None
          else
            Some r
      end

    let merge_contexts_exn (rc1:t) (rc2:t) : t =
      let rc2_list = D.as_kvp_list rc2 in
      insert_list_exn rc1 (List.map ~f:(fun (n,(r,b)) -> (n,r,b)) rc2_list)

    let compare (rc1:t) (rc2:t) : comparison =
      D.compare rc1 rc2

    let to_string : t -> string = D.to_string

    let hash (rc:t) : int =
      let kvp_list = D.as_kvp_list rc in
      List.foldi
        ~f:(fun i acc (id,(r,abs)) ->
            (regex_hash r)
            lxor (Bool.hash abs)
            lxor (String.hash id)
            lxor (Int.hash i)
            lxor acc)
        ~init:(-25389029)
        kvp_list
end

(***** }}} *****)
