let drop l n = 
  let rec acc x = function 
    | ([],   _ ) -> x
    | (h::t, 1 ) -> acc x (t, n)
    | (h::t, n') -> acc (h::x) (t, n'-1)
  in List.rev (acc [] (l, n))
;;

assert (drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]);;
