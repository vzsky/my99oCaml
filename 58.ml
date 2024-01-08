type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

(* From 56 *)

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

(* From 55 *)

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

(* Solution *)

let sym_cbal_trees n = 
  List.filter is_symmetric (cbal_tree n)
;;

(* Test *)
let rec range a b = if a > b then [] else if a = b then [b] else a::(range (a+1) b);;

assert (List.length (sym_cbal_trees 57) = 256);;
assert (List.map (fun n -> n, List.length(sym_cbal_trees n)) (range 10 20) 
  = [(10, 0); (11, 4); (12, 0); (13, 4); (14, 0); (15, 1); (16, 0); (17, 8); (18, 0); (19, 16); (20, 0)])


