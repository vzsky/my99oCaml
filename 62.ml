type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec internals t = 
  match t with 
  | Empty                  -> []
  | Node (x, Empty, Empty) -> []
  | Node (x, l, r)         -> x::((internals l) @ (internals r))
;;

let tree = 
  (Node ('a', 
    Node ('b',
      Node ('d', Empty, Empty), 
      Node ('e', Empty, Empty)),
    Node ('c', 
      Empty, 
      Node ('f', 
        Node ('g', Empty, Empty), 
        Empty)))) ;;

assert (internals (Node ('a', Empty, Empty)) = []);;
assert (internals tree = ['a'; 'b'; 'c'; 'f']);;

let rec at_level t lv = 
  match t with 
  | Empty -> []
  | Node (x, l, r) -> 
    if lv = 1 then [x] 
    else (at_level l (lv-1))@(at_level r (lv-1))
;;

assert (at_level tree 2 = ['b'; 'c']);;
assert (at_level tree 5 = []);;
