type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

(* From 60 *)
let max_nodes h = 1 lsl h - 1;;
let min_height n = 
  let rec check a = 
    if max_nodes a >= n then a 
    else check (a+1)
  in check 0
;;

(* Solution *)

let num l = (1 lsl (l-1)) - 1;;

let divide = 
  let rec p acc l n =
    match l with 
    | []   -> failwith "cannot correctly partition list"
    | h::t -> 
      if n = 0 then (List.rev acc, h, t) 
      else p (h::acc) t (n-1)
  in p []
;;

let complete_binary_tree lst = 
  let lev = min_height (List.length lst) in 
  let rec make lv ls =  
    match ls with 
    | [] -> Empty
    | _  -> 
      let (l, n, r) = divide ls (num lv) in 
      Node (n, (make (lv-1) l), (make (lv-1) r))
    in 
  make lev lst 
;;

assert (complete_binary_tree [1; 2; 3; 4; 5; 6] = 
  Node (4, 
    Node (2, 
      Node (1, Empty, Empty),                         (*       4       *)
      Node (3, Empty, Empty)),                        (*   2       6   *)
    Node (6,                                          (* 1   3   5   _ *)
      Node (5, Empty, Empty), 
      Empty)));;
