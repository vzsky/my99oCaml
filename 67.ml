type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec string_of_tree (t: char binary_tree) = 
  match t with 
  | Empty                   -> ""
  | Node (a, Empty, Empty)  -> String.make 1 a
  | Node (a, l, r)          -> (String.make 1 a) ^ "(" ^ (string_of_tree l) ^ "," ^ (string_of_tree r) ^ ")"
;;

let tree_of_string s = 
  if s = "" then Empty else 
  let rec parse i = (* recieve start parse, return next parsed *)
    if (i > String.length s) || (s.[i] = ')') || (s.[i] = ',') then 
      (Empty, i+1)
    else if ((i + 1) < String.length s) && (s.[i+1] = '(') then 
      let (l, li) = parse (i+2) in 
      let (r, ri) = parse li in 
      (Node (s.[i], l, r), ri+1)
    else 
      (Node (s.[i], Empty, Empty), i+2)
  in fst (parse 0) 
;;
    
     

(* Tests *)

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  (Node ('a', 
    Node ('b', 
      leaf 'd', 
      leaf 'e'),
    Node ('c', 
      Empty, 
      Node ('f', 
        leaf 'g', 
        Empty))));;

assert (string_of_tree example_layout_tree = "a(b(d,e),c(,f(g,)))" );;
assert (tree_of_string "a(b(d,e),c(,f(g,)))" = example_layout_tree);;
assert (tree_of_string "" = Empty);;
