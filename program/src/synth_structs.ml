open Lang
open Util
open String_utilities

type queue_element =
  {
    r1 : regex;
    r2 : regex;
    expansions_performed : int;
    expansions_inferred : int;
    expansions_forced : int;
  }

let nqe_to_tuple
    (q:queue_element)
  : regex * regex * int * int * int =
  (q.r1,
   q.r2,
   q.expansions_performed,
   q.expansions_inferred,
   q.expansions_forced)
  

let queue_element_comparison
  (q1:queue_element)
  (q2:queue_element)
  : comparison =
  quint_compare
    regex_compare
    regex_compare
    (fun _ _ -> EQ)
    (fun _ _ -> EQ)
    (fun _ _ -> EQ)
    (nqe_to_tuple q1)
    (nqe_to_tuple q2)


let queue_element_to_string =
  (string_of_quintuple
    regex_to_string
    regex_to_string
    string_of_int
    string_of_int
    string_of_int)
  % nqe_to_tuple
