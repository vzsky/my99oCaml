type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec count_leaves t = 
  match t with 
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) -> (count_leaves l) + (count_leaves r)
;;

assert (count_leaves Empty = 0);;
assert (count_leaves (Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
  Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))) = 3);;

let rec leaves t = 
  match t with 
  | Empty -> []
  | Node (x, Empty, Empty) -> [x]
  | Node (_, l, r) -> (leaves l)@(leaves r)
;;

assert (leaves Empty = []);;
assert (leaves (Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
  Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))) = ['d'; 'e'; 'g']);;
