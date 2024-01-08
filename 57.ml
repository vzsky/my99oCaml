type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec construct lst = 
  let gt h t = List.filter (fun x -> x > h) t in
  let ls h t = List.filter (fun x -> x < h) t in
  match lst with 
  | []   -> Empty
  | h::t -> Node (h, construct (gt h t), construct (ls h t))
;;

(* Just for test, From 56 *)
let rec is_mirror l r = 
  match l, r with 
  | Empty, Empty -> true 
  | Node (_, ll, lr), Node (_, rl, rr) -> is_mirror ll rr && is_mirror lr rl 
  | _ -> false
;;

let is_symmetric t = 
  match t with 
  | Empty -> true 
  | Node (_, l, r) -> is_mirror l r
;; 

assert (is_symmetric (construct [5; 3; 18; 1; 4; 12; 21]));;
assert ( not (is_symmetric (construct [3; 2; 5; 7; 4])));;
