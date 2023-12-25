let encode l = 
  let rec acc a x = function 
    | []   -> [(x, a)]
    | h::t -> if a=h then acc a (x+1) t else (x, a) :: (acc h 1 t)
  in match l with 
    | []   -> [] 
    | h::t -> acc h 1 t
;;

assert (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] 
  = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] ) ;;
