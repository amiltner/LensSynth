open Core.Std

type 'a t =
  | Leaf of 'a
  | Node of 'a t list

let to_list (r : 'a t) : 'a list =
  let rec to_list_internal (r : 'a t) (acc : 'a list) =
    begin match r with
    | Leaf d -> d::acc
    | Node nl ->
        List.fold_left
          ~f:(fun acc n -> to_list_internal n acc)
          ~init:acc
          nl
    end
  in
  to_list_internal r []

