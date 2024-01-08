type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let x = 'x';;

let tree ls rs = 
  let tree_l l rs = 
    List.map (fun r -> Node (x, l, r)) rs in 
  List.fold_left (fun acc l -> (tree_l l rs)@acc) [] ls
;;

let rec hbal_tree h = 
  if h = 0 then [Empty]
  else if h = 1 then [Node (x, Empty, Empty)]
  else 
    let t1 = hbal_tree (h-1) in 
    let t2 = hbal_tree (h-2) in 
    (tree t1 t2) @ (tree t2 t1) @ (tree t1 t1)
;;
    

(* Test *)
let t = hbal_tree 3;; 

assert (List.length t = 15);;
assert (List.mem 
  (Node (x, Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)), Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty))))
  t);;

assert (List.mem 
  (Node (x, Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)), Node (x, Node (x, Empty, Empty), Empty))) 
  t);;
