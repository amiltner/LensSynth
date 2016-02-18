open Core.Std

(*let rec cartesian_product (l:'a list list) : 'a list list =
  begin match l with
  | [] -> [[]]
  | x::xs -> let rest = cartesian_product xs in
    List.concat (List.map (fun i -> List.map (fun rs -> i :: rs) rest) x)
  end*)

let cartesian_map (f:'a -> 'b -> 'c) (l1:'a list) (l2:'b list) : 'c list =
  (List.fold_left
    ~f:(fun acc x ->
      (List.fold_left
        ~f:(fun acc2 y ->
          (f x y)::acc2)
        ~init:[]
        l2)@acc)
    ~init:[]
    l1)
