type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

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

assert (is_symmetric (Node ('a', (Node ('b', Empty, Empty)), (Node ('c', Empty, Empty)))));;
assert (is_symmetric 
  (Node ('a', 
    (Node ('b', 
      (Node ('a', Empty, Empty)), 
      (Empty))), 
    (Node ('c', 
      (Empty), 
      (Node ('b', Empty, Empty)))))));;
assert (not (is_symmetric (Node ('a', Empty, Node ('b', Empty, Empty)))));;
