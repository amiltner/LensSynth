open Core.Std
open Fasteval
open Util
open Lang
open Lens
open Eval
open Util
open Pp
open Permutation

let empty_string = RegExBase ""

let rec exponentiate (r:regex) (n:int) : regex =
  if n < 0 then
    failwith "invalid exponential"
  else if n = 0 then
    empty_string
  else
    RegExConcat
      (exponentiate r (n-1)
      ,r)

let rec quotiented_star (r:regex) (n:int) : regex =
  if n < 1 then
    failwith "invalid modulation"
  else if n = 1 then
    empty_string
  else
    RegExOr
      ((quotiented_star r (n-1))
      ,(exponentiate r (n-1)))

let rec empty_or_not_star_expansion (r:regex) : regex =
  RegExOr
    (RegExBase ""
    ,RegExConcat
      (r
      ,RegExStar r))

let rec quotient_product_expansion (n:int) (r:regex) : regex =
  RegExConcat
    ((quotiented_star r n)
    ,(RegExStar
      (exponentiate r n)))

let rec expand_stars (r:regex) (n:int) (max_size:int) : regex list =
  if n = 0 then
    [r]
  else
    let relevant_primes = primes_beneath_n max_size in
    let transformations = empty_or_not_star_expansion::
      (List.map
        ~f:(fun p -> quotient_product_expansion p)
        relevant_primes) in
    let rec expand_stars_internal (r:regex) : regex list =
      begin match r with
      | RegExBase _ -> []
      | RegExConcat (r1,r2) ->
          let r1_expansions = expand_stars_internal r1 in
          let r2_expansions = expand_stars_internal r2 in
          (List.map
            ~f:(fun expansion -> RegExConcat (expansion,r2))
            r1_expansions)
          @
          (List.map
            ~f:(fun expansion -> RegExConcat (r1,expansion))
            r2_expansions)
      | RegExOr (r1,r2) ->
          let r1_expansions = expand_stars_internal r1 in
          let r2_expansions = expand_stars_internal r2 in
          (List.map
            ~f:(fun expansion -> RegExOr (expansion,r2))
            r1_expansions)
          @
          (List.map
            ~f:(fun expansion -> RegExOr (r1,expansion))
            r2_expansions)
      | RegExStar (r') ->
          List.map ~f:(fun t -> t r') transformations
          @ (List.map ~f:(fun r'' -> RegExStar r'') (expand_stars_internal r'))
      | RegExUserDefined _ -> []
      end
    in
    List.fold_left
    ~f:(fun acc k ->
      List.dedup
        (List.concat (List.map ~f:(fun x -> expand_stars_internal x) acc)))
    ~init:[r]
    (range 0 (n-1))
