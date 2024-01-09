type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec depth tree = 
  match tree with 
  | Empty -> 0 
  | Node (_, l, r) -> (max (depth l) (depth r)) + 1
;;

let rec left_depth tree = 
  match tree with 
  | Empty -> 0
  | Node (_, l, _) -> (left_depth l) + 1
;;

let layout_binary_tree_2 tree = 
  let dep = depth tree in 
  let leng d = 1 lsl (dep - d - 1) in
  let leftdep = left_depth tree in 
  let start = ( 1 lsl leftdep ) - 1 in

  let rec make t x y= 
    match t with 
    | Empty           -> Empty 
    | Node (a, l, r)  -> Node ((a, x, y), (make l (x - leng y) (y+1)), (make r (x + leng y) (y+1)))
  in 
  make tree start 1 
;;

(* Test *)
let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', 
    Node ('k', 
      Node ('c', 
        leaf 'a',
        Node ('e', 
          leaf 'd', 
          leaf 'g')),
      leaf 'm'),
    Node ('u', 
      Node ('p', 
        Empty, 
        leaf 'q'), 
      Empty));;

assert (layout_binary_tree_2 example_layout_tree = 
  Node (('n', 15, 1),
    Node (('k', 7, 2),
      Node (('c', 3, 3), 
        Node (('a', 1, 4), Empty, Empty),
        Node (('e', 5, 4), 
          Node (('d', 4, 5), Empty, Empty),
          Node (('g', 6, 5), Empty, Empty))),
      Node (('m', 11, 3), Empty, Empty)),
    Node (('u', 23, 2),
      Node (('p', 19, 3), 
        Empty, 
        Node (('q', 21, 4), Empty, Empty)), 
      Empty)));;

let example2_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', 
    Empty,
    Node ('u', 
      Node ('p', 
        Empty, 
        leaf 'q'), 
      Empty));;

assert (layout_binary_tree_2 example2_layout_tree = 
  Node (('n', 1, 1), 
    Empty,
    Node (('u', 5, 2),
      Node (('p', 3, 3), 
        Empty, 
        Node (('q', 4, 4), Empty, Empty)), 
      Empty)));;
