open Core
open Util
open String_utilities
open Lang

(***** The main RegexContext module {{{ *****)

module type RegexContext_Sig = sig
    type t

    val empty                    : t
    val lookup                   : t -> id -> Regex.t option
    val lookup_exn               : t -> id -> Regex.t
    val insert_exn               : t -> id -> Regex.t -> bool -> t
    val insert_list_exn          : t -> (id * Regex.t * bool) list -> t
    val create_from_list_exn     : (id * Regex.t * bool) list -> t
    val lookup_for_expansion_exn : t -> id -> Regex.t option
    val autogen_id               : t -> Regex.t -> id
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
      type value = Regex.t * bool
      let compare_key = compare_id
      let compare_value = pair_compare Regex.compare compare
      let key_to_string = show_id
      let value_to_string = string_of_pair Regex.show string_of_bool
    end)
    type t = D.dict

    let empty = D.empty

    let lookup_everything (rc:t) (name:id) : (Regex.t*bool) option =
      D.lookup rc name

    let lookup (rc:t) (name:id) : Regex.t option =
      Option.map ~f:fst (lookup_everything rc name)

    let lookup_exn (rc:t) (name:id) : Regex.t =
      begin match lookup rc name with
        | None -> failwith "lookup_exn: key not found"
        | Some v -> v
      end

    let insert_exn (rc:t) (name:id) (r:Regex.t) (is_abstract:bool) : t =
      begin match lookup_everything rc name with
        | None -> D.insert rc name (r,is_abstract)
        | Some ra ->
            if ra = (r,is_abstract) then
              rc
            else
              failwith ((show_id name) ^ " already exists in the context")
      end

    let insert_list_exn (rc:t) (nral:(id * Regex.t * bool) list) : t =
      List.fold_left
        ~f:(fun acc (name,r,b) -> insert_exn acc name r b)
        ~init:rc
        nral

    let create_from_list_exn (nral:(id * Regex.t * bool) list) : t =
      insert_list_exn empty nral

    let autogen_id (rc:t) (r:Regex.t) : id =
      let base = Regex.show r in
      let rec fresh n =
        let x = Id (Printf.sprintf "%s%d" base n) in
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
        let x = Id (Printf.sprintf "%s%d" base n) in
        begin match D.lookup rc x with
          | Some _ -> fresh (n+1)
          | _ -> x
        end
      in
      fresh 1

    let lookup_for_expansion_exn (rc:t) (name:id) : Regex.t option =
      begin match lookup_everything rc name with
        | None -> failwith ("bad regex name: " ^ (show_id name))
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
            (Regex.hash r)
            lxor (Bool.hash abs)
            lxor (hash_id id)
            lxor (Int.hash i)
            lxor acc)
        ~init:(-25389029)
        kvp_list
end

(***** }}} *****)
