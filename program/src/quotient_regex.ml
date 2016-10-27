open Lang
open Permutation
open Core.Std

type quotient_regex =
  | QuotientRegExEmpty
  | QuotientRegExBase of string
  | QuotientRegExConcat of quotient_regex * quotient_regex
  | QuotientRegExOr of quotient_regex * quotient_regex
  | QuotientRegExStar of quotient_regex
  | QuotientRegExVariable of string
  | QuotientRegExPermute of quotient_regex list * regex
  | QuotientRegExMap of regex * string

(* Convert an ordinary regex into a quotient regex *)
let rec promote_regex r =
  match r with
  | RegExEmpty -> QuotientRegExEmpty
  | RegExBase s -> QuotientRegExBase s
  | RegExConcat (r1, r2) -> QuotientRegExConcat (promote_regex r1, promote_regex r2)
  | RegExOr (r1, r2) -> QuotientRegExOr (promote_regex r1, promote_regex r2)
  | RegExStar r -> QuotientRegExStar (promote_regex r)
  | RegExVariable s -> QuotientRegExVariable s

(* This function generates a quotient regex that concatenates the elements
 * of l with our separator r in between *)
let rec intersperse l sep =
  match l with
  | [] -> QuotientRegExBase ""
  | [h] -> h
  | h :: t -> QuotientRegExConcat (h, QuotientRegExConcat (promote_regex sep, intersperse t sep))

let rec kernel q =
  match q with
  | QuotientRegExEmpty -> RegExEmpty
  | QuotientRegExBase s -> RegExBase s
  | QuotientRegExConcat (q1, q2) -> RegExConcat (kernel q1, kernel q2)
  | QuotientRegExOr (q1, q2) -> RegExOr (kernel q1, kernel q2)
  | QuotientRegExStar q -> RegExStar (kernel q)
  | QuotientRegExVariable s -> RegExVariable s
  | QuotientRegExMap (_, s) -> RegExBase s
  | QuotientRegExPermute (l, sep) -> kernel (intersperse l sep)

let rec whole q =
  match q with
  | QuotientRegExEmpty -> RegExEmpty
  | QuotientRegExBase s -> RegExBase s
  | QuotientRegExConcat (q1, q2) -> RegExConcat (whole q1, whole q2)
  | QuotientRegExOr (q1, q2) -> RegExOr (whole q1, whole q2)
  | QuotientRegExStar q -> RegExStar (whole q)
  | QuotientRegExVariable s -> RegExVariable s
  | QuotientRegExMap (r, _) -> r
  | QuotientRegExPermute (l, sep) -> 
      List.fold_left ~init:(RegExEmpty) 
        ~f:(fun acc x -> if acc = RegExEmpty then x else RegExOr (x, acc))
        (List.map ~f:(fun x -> kernel (intersperse x sep)) 
          (List.map ~f: (fun p -> Permutation.apply_to_list_exn p l) 
          (Permutation.create_all (List.length l))))
