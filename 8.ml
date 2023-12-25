let rec rp a l = match l with 
  | []   -> []
  | h::t -> if h = a then rp a t else l
;;

let rec compress = function 
  | []   -> []
  | h::t -> h:: compress (rp h t)
;;

let rec compress' = function 
  | a::(b::c) -> if a=b then compress' (b::c) else a::(compress (b::c))
  | other     -> other
;;

assert ( compress  ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "a"; "d"; "e"] ) ;;
assert ( compress' ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "a"; "d"; "e"] ) ;;
