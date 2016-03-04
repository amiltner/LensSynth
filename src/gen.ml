open Core.Std
open Lang
open Lens
open Eval
open Util
open Pp

let rec all_match (c:context) (r:regex) (ss:string list) : bool =
  List.fold_left
  ~f:(fun acc s -> acc && eval_regex c r s)
  ~init:true
  ss

let rec gen_normalized_lenses (c:context) (s:normalized_regex)
(t:normalized_regex) (exs:examples) : normalized_lens list =
  []

let rec gen_lenses (c:context) (s:regex) (t:regex) (exs:examples) : lens list =
  let structural_lenses = (begin match (s,t) with
  | (RegExBase s1, RegExBase s2) ->
      let (exss,exst) = List.unzip exs in
      if ((all_match c s exss) && (all_match c t exst)) then
        [ConstLens (s1, s2)]
      else
        []
  | (RegExConcat (s1,s2), RegExConcat (t1,t2)) ->
      let gen_concat permutation1 permutation2 generator = (
        let (sexs,texs) = List.unzip exs in
        let sexs_splits = List.map
          ~f:(retrieve_regex_concat_split c s1 s2)
          sexs in
        let texs_splits = List.map
          ~f:(retrieve_regex_concat_split c t1 t2)
          texs in
        begin match (distribute_option sexs_splits, distribute_option texs_splits) with
        | (Some sexsplits, Some texsplits) ->
          let (t1,t2) = permutation1 (t1,t2) in
          let (sexs1,sexs2) = List.unzip sexsplits in
          let (texs1,texs2) = permutation2 (List.unzip texsplits) in
          let lenses1 = gen_lenses c s1 t1 (List.zip_exn sexs1 texs1) in
          let lenses2 = gen_lenses c s2 t2 (List.zip_exn sexs2 texs2) in
          cartesian_map
            generator
            lenses1
            lenses2
        | _ -> []
        end
      ) in
      gen_concat
        (fun (x,y) -> (x,y))
        (fun (x,y) -> (x,y))
        (fun l1 l2 -> ConcatLens (l1,l2)) @
      gen_concat
        (fun (x,y) -> (y,x))
        (fun (x,y) -> (y,x))
        (fun l1 l2 -> SwapLens (l1,l2))
  | (RegExOr (s1,s2), RegExOr (t1,t2)) ->
      let (sexs,texs) = List.unzip exs in
      let sexs_choices = List.map
        ~f:(retrieve_regex_or_choice c s1 s2)
        sexs in
      let texs_choices = List.map
        ~f:(retrieve_regex_or_choice c t1 t2)
        texs in
      begin match (distribute_option sexs_choices, distribute_option texs_choices) with
      | (Some sexchoices, Some texchoices) ->
          let split_examples = List.fold_left
          ~f:(fun (accs) ((sexchoice,texchoice),ex) ->
            begin match accs with
            | None -> None
            | Some (leftacc,rightacc) ->
                begin match (sexchoice,texchoice) with
                | (false,false) -> Some (ex::leftacc,rightacc)
                | (true,true) -> Some (leftacc,ex::rightacc)
                | _ -> None
                end
            end)
          ~init:(Some ([],[]))
          (List.zip_exn (List.zip_exn sexchoices texchoices) exs) in
          begin match split_examples with
          | None -> []
          | Some (exs1,exs2) ->
              let ls1 = gen_lenses c s1 t1 exs1 in
              let ls2 = gen_lenses c s2 t2 exs2 in
              cartesian_map
                (fun x y -> UnionLens (x,y))
                ls1
                ls2
          end
      | _ -> []
      end
    
  | (RegExStar s', RegExStar t') ->
      let (sexs,texs) = List.unzip exs in
      let sexs_splits = List.map
        ~f:(retrieve_regex_star_splits c s')
        sexs in
      let texs_splits = List.map
        ~f:(retrieve_regex_star_splits c t')
        texs in
      begin match (distribute_option sexs_splits, distribute_option texs_splits) with
      | (Some sexs_splits,Some texs_splits) ->
          let split_examples = List.fold_left
          ~f:(fun (accs) ((sexsplit,texsplit)) ->
            begin match accs with
            | None -> None
            | Some (acc) ->
                (begin match (List.zip sexsplit texsplit) with
                | None -> None
                | Some zip -> Some (zip @ acc)
                end)
            end)
          ~init:(Some [])
          (List.zip_exn sexs_splits texs_splits) in

          begin match split_examples with
          | None -> []
          | Some l ->
              let ls' = gen_lenses c s' t' l in
              List.map
              ~f:(fun x -> IterateLens x)
              ls'
          end
      | _ -> []
      end
  | (RegExUserDefined typ1, RegExUserDefined typ2) -> []
  | _ -> []
  end) in
  let (sexs,texs) = List.unzip exs in
  if (s = t
      && sexs = texs
      && all_match c s sexs) then
        IdentityLens :: structural_lenses
      else
        structural_lenses
