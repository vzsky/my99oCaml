type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let layout_binary_tree_1 tree = 
  let rec layout t i d = 
    match t with 
    | Empty           -> (Empty, i)
    | Node (a, l, r)  -> 
      let (left, il) = layout l i      (d+1) in 
      let (righ, ir) = layout r (il+1) (d+1) in 
      (Node((a, il+1, d), left, righ), ir) 
  in
  fst (layout tree 0 1)
;;

(* Test *)

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', 
    Node ('k', 
      Node ('c', 
        leaf 'a',
        Node ('h', 
          Node ('g', 
            leaf 'e', 
            Empty), 
          Empty)),
      leaf 'm'),
    Node ('u', 
      Node ('p', 
        Empty, 
        Node ('s', 
          leaf 'q', 
          Empty)), 
      Empty));;

assert (layout_binary_tree_1 example_layout_tree = 
  Node (('n', 8, 1),
    Node (('k', 6, 2),
      Node (('c', 2, 3), 
        Node (('a', 1, 4), Empty, Empty),
        Node (('h', 5, 4),
          Node (('g', 4, 5), 
            Node (('e', 3, 6), Empty, Empty), 
            Empty), 
          Empty)),
      Node (('m', 7, 3), Empty, Empty)),
    Node (('u', 12, 2),
      Node (('p', 9, 3), Empty,
        Node (('s', 11, 4), 
          Node (('q', 10, 5), Empty, Empty), 
          Empty)),
      Empty)));;
