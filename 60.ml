(* From 59 *)
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

(* Solution *)

let max_nodes h = 1 lsl h - 1;;
let rec min_nodes h = 
  if h = 0 then 0 
  else if h = 1 then 1 
  else (min_nodes (h-1)) + (min_nodes (h-2)) + 1 
;;

let min_height n = 
  let rec check a = 
    if max_nodes a >= n then a 
    else check (a+1)
  in check 0
;;

let max_height n = 
  let rec check a = 
    if min_nodes a > n then a - 1 
    else check (a+1)
  in check 0 
;;

let rec nodes = function 
  | Empty          -> 0 
  | Node (_, l, r) -> 1 + nodes l + nodes r
;;

let rec range a b = if a > b then [] else if a = b then [b] else a::(range (a+1) b);;

let hbal_tree_nodes n = 
  let rng = range (min_height n) (max_height n) in 
  let all = List.fold_left (fun acc cur -> (hbal_tree cur)@acc) [] rng in 
  List.filter (fun x -> nodes x = n) all
;;


(* Tests *)
assert (List.length (hbal_tree_nodes 15) = 1553);;
assert (List.map hbal_tree_nodes [0; 1; 2; 3] = 
  [ [Empty]; 
    [Node ('x', Empty, Empty)];
    [Node ('x', Node ('x', Empty, Empty), Empty); Node ('x', Empty, Node ('x', Empty, Empty))];
    [Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))] ] );;
