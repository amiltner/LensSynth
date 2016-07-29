open Core.Std
open Lang
open Regex
open Datastructures

(***** The main RegexContext module {{{ *****)

module type RegexContext_Sig = sig
    type t

    val empty                    : t
    val lookup_exn               : t -> id -> regex
    val insert_exn               : t -> id -> regex -> bool -> t
    val insert_list_exn          : t -> (id * regex * bool) list -> t
    val create_from_list_exn     : (id * regex * bool) list -> t
    val lookup_for_expansion_exn : t -> id -> regex option
    val fresh_id                 : t -> id
    val autogen_id               : t -> regex -> id
    val merge_contexts_exn       : t -> t -> t
end

module RegexContext_Struct (Dict : Dictionary) : RegexContext_Sig = struct
    type t = (id, (regex*bool)) Dict.t

    let empty = Dict.empty (=)

    let lookup (rc:t) (name:id) : (regex*bool) option =
      Dict.find name rc

    let lookup_exn (rc:t) (name:id) : regex =
      fst (Dict.find_exn name rc)

    let insert_exn (rc:t) (name:id) (r:regex) (is_abstract:bool) : t =
      begin match lookup rc name with
        | None -> Dict.set name (r,is_abstract) rc
        | Some ra ->
            if ra = (r,is_abstract) then
              rc
            else
              failwith "bad insert"
      end

    let insert_list_exn (rc:t) (nral:(id * regex * bool) list) : t =
      List.fold_left
        ~f:(fun acc (name,r,b) -> insert_exn acc name r b)
        ~init:rc
        nral

    let create_from_list_exn (nral:(id * regex * bool) list) : t =
      insert_list_exn empty nral

    let fresh_id (rc:t) : id =
      let rec fresh n =
        let x = Printf.sprintf "%d" n in
        begin match Dict.find x rc with
          | Some _ -> fresh (n+1)
          | _ -> x
        end
      in
      fresh 1

    let autogen_id (rc:t) (r:regex) : id =
      let base = "U" ^ Pp.pp_regexp r in
      let rec fresh n =
        let x = Printf.sprintf "%s%d" base n in
        begin match Dict.find x rc with
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

    let lookup_for_expansion_exn (rc:t) (name:id) : regex option =
      begin match lookup rc name with
        | None -> failwith "bad regex name"
        | Some (r,abs) ->
          if abs then
            None
          else
            Some r
      end

    let merge_contexts_exn (rc1:t) (rc2:t) : t =
      let rc2_list = Dict.as_kvp_list rc2 in
      insert_list_exn rc1 (List.map ~f:(fun (n,(r,b)) -> (n,r,b)) rc2_list)
end

module RegexContext = RegexContext_Struct(ListDictionary)

(***** }}} *****)
