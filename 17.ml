let split = 
  let rec s' x y n = 
    match y with 
    | []   -> (List.rev x, y)
    | h::t -> if n=0 then (List.rev x, y) else s' (h::x) t (n-1)
  in s' []
;;

assert (split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 
  = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])) ;;

assert (split ["a"; "b"; "c"; "d"] 5 
  = (["a"; "b"; "c"; "d"], [])) ;;
