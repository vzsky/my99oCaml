let rotate l n= 
  let rec r x = function 
    | []   -> []
    | h::t -> if x=0 then (h::t) else r (x-1) (t @ [h])
  in r (List.length l + n) l
;;


assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]) ;;
assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]) ;;
