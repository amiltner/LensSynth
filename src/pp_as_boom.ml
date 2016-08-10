open Regex
open Lens
open Format

(***** Helpers  *****)

let fpf         = fprintf ;;
let ident ppf s = fpf ppf "%s" s ;;
let kwd   ppf s = fpf ppf "%s" s ;;

(*****  *****)

(***** Regexs  *****)

let desugar_string (s:string) =
  Str.global_replace (Str.regexp "\"") "\\\\\""
    (Str.global_replace (Str.regexp "\n") "\\\\n"
        (Str.global_replace (Str.regexp "\\\\") "\\\\\\\\" s))

let prec_of_regex (r:regex) =
  begin match r with
    | RegExEmpty      -> 150
    | RegExBase     _ -> 150
    | RegExVariable _ -> 150
    | RegExMapped   _ -> 150
    | RegExStar     _ -> 125
    | RegExConcat   _ -> 100
    | RegExOr       _ -> 75
  end

let prec_of_lens (l:lens) =
  begin match l with
    | LensConst    _ -> 150
    | LensIdentity _ -> 150
    | LensVariable _ -> 150
    | LensInverse  _ -> failwith "TODO: inverse support"
    | LensIterate  _ -> 125
    | LensSwap     _ -> 112
    | LensCompose  _ -> 112
    | LensConcat   _ -> 100
    | LensUnion    _ -> 75
  end

let rec boom_fpf_regex
    (ppf:formatter)
    ((lvl, r, or_seq, conc_seq):int * regex * bool * bool)
  : unit =
  let this_lvl = prec_of_regex r in
  (if this_lvl < lvl then fpf ppf "(");
  begin match r with
    | RegExEmpty -> fpf ppf "AHHH" 
    | RegExBase s -> fpf ppf "\"%a\"" ident (desugar_string s)
    | RegExVariable n -> fpf ppf "%a" ident n
    | RegExMapped _ -> failwith "ahhhh"
    | RegExStar r' -> fpf ppf "%a* " boom_fpf_regex (this_lvl, r', false, false)
    | RegExConcat (r1,r2) ->
      if conc_seq then
        fpf ppf "@[<-2>%a@ . %a@]"
          boom_fpf_regex (this_lvl, r1, false, true)
          boom_fpf_regex (this_lvl, r2, false, true)
      else
        fpf ppf "@[<2>%a@ . %a@]"
          boom_fpf_regex (this_lvl, r1, false, true)
          boom_fpf_regex (this_lvl, r2, false, true)
    | RegExOr (r1,r2) ->
      if or_seq then
        fpf ppf "@[<-2>%a@ | %a@]"
          boom_fpf_regex (this_lvl, r1, true, false)
          boom_fpf_regex (this_lvl, r2, true, false)
      else
        fpf ppf "@[<2>%a@ | %a@]"
          boom_fpf_regex (this_lvl, r1, true, false)
          boom_fpf_regex (this_lvl, r2, true, false)
  end;
  (if this_lvl < lvl then fpf ppf ")")

let rec boom_fpf_lens
    (ppf:formatter)
    ((lvl, l, or_seq, conc_seq):int * lens * bool * bool)
  : unit =
  let this_lvl = prec_of_lens l in
  (if this_lvl < lvl then fpf ppf "(");
  begin match l with
    | LensConst (s1,s2) ->
      let s1 = desugar_string s1 in
      let s2 = desugar_string s2 in
      if s1 <> "" && s2 <> "" then
        fpf ppf "del \"%a\"@ . ins \"%a\""
          ident s1
          ident s2
      else if s1 = "" then
        fpf ppf "ins \"%a\""
          ident s2
      else if s2 = "" then
        fpf ppf "del \"%a\""
          ident s1
    | LensConcat(l1,l2) ->
      if conc_seq then
        fpf ppf "@[<-2>%a@ . %a@]"
          boom_fpf_lens (this_lvl, l1, false, true)
          boom_fpf_lens (this_lvl, l2, false, true)
      else
        fpf ppf "@[<2>%a@ . %a@]"
          boom_fpf_lens (this_lvl, l1, false, true)
          boom_fpf_lens (this_lvl, l2, false, true)
    | LensSwap(l1,l2) ->
      fpf ppf "@[<2>lens_swap@ %a@ %a@]"
        boom_fpf_lens (this_lvl+1, l1, false, false)
        boom_fpf_lens (this_lvl+1, l2, false, false)
    | LensUnion(l1,l2) ->
      if or_seq then
        fpf ppf "@[<-2>%a@ || %a@]"
          boom_fpf_lens (this_lvl, l1, true, false)
          boom_fpf_lens (this_lvl, l2, true, false)
      else
        fpf ppf "@[<2>%a@ || %a@]"
          boom_fpf_lens (this_lvl, l1, false, false)
          boom_fpf_lens (this_lvl, l2, true, false)
    | LensCompose(l1,l2) ->
      fpf ppf "@[<2>compose@ %a@ %a@]"
        boom_fpf_lens (this_lvl+1, l1, false, false)
        boom_fpf_lens (this_lvl+1, l2, false, false)
    | LensIterate(l') ->
      fpf ppf "%a* "
        boom_fpf_lens (this_lvl, l', false, false);
    | LensIdentity r ->
      boom_fpf_regex ppf (this_lvl,r, false, false)
    | LensInverse _ ->
      failwith "inverse doesn't exist in boom"
    | LensVariable n ->
      fpf ppf "%a"
        ident
        n
  end;
  (if this_lvl < lvl then fpf ppf ")")


let boom_pp_regex (r:regex) : string =
  (boom_fpf_regex str_formatter (0, r, false, false); flush_str_formatter ())

let boom_pp_lens (l:lens) : string =
  (boom_fpf_lens str_formatter (0, l, false, false); flush_str_formatter ())
