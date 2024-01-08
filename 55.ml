type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let tree ls rs = 
  let tree_l l rs = 
    List.map (fun r -> Node ('x', l, r)) rs in 
  List.fold_left (fun acc l -> (tree_l l rs)@acc) [] ls
;;

let rec cbal_tree n = 
  if n = 0 then [Empty]
  else if n mod 2 = 1 then 
    let hf = cbal_tree (n/2) in 
    tree hf hf
  else 
    let hf = cbal_tree (n/2) in 
    let hg = cbal_tree ((n/2)-1) in 
    (tree hf hg) @ (tree hg hf)
;;

assert (cbal_tree 4 = 
  [ Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)), Node ('x', Empty, Empty));
    Node ('x', Node ('x', Node ('x', Empty, Empty), Empty), Node ('x', Empty, Empty));
    Node ('x', Node ('x', Empty, Empty), Node ('x', Node ('x', Empty, Empty), Empty));
    Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Node ('x', Empty, Empty))) ]);;

assert (List.length (cbal_tree 40) = 524288);;
