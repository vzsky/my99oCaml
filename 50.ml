type tree = 
  | Node of tree * tree
  | Leaf of string
;;

let make_leaf (a, b) = (Leaf a, b);; 
let combine a b = (Node ((fst a), (fst b)), (snd a) + (snd b));;

let rec heap_add heap = fun x -> 
  match heap with 
  | []   -> [x]
  | h::t -> if (snd x) < (snd h) then x::h::t else h::(heap_add t x)
;;

let huffman lst = 
  let heap = List.fold_left (fun acc cur -> heap_add acc (make_leaf cur)) [] lst in 
  let rec get_tree hp = 
    match hp with 
    | []   -> None
    | [x]  -> Some (fst x)
    | a::b::t -> get_tree (heap_add t (combine a b)) in
  let rec traverse tr st = 
    match tr with
    | Leaf s      -> [(s, st)]
    | Node (l, r) -> (traverse l (st^"0")) @ (traverse r (st^"1"))
  in 
  let tree = get_tree heap in  
  match tree with 
  | None   -> []
  | Some t -> traverse t ""
;;

assert (huffman [("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)] 
  = [("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101"); ("d", "111")]);;

assert (huffman [("a", 10); ("b", 15); ("c", 30); ("d", 16); ("e", 29)] 
  = [("d", "00"); ("a", "010"); ("b", "011"); ("e", "10"); ("c", "11")]);;
