type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

let encode l = 
  let rec acc a x = function 
    | []   -> [ if x=1 then One a else Many (x, a) ]
    | h::t -> if h=a then acc a (x+1) t else (if x=1 then One a else Many (x, a)) :: (acc h 1 t)
  in match l with 
    | []   -> []
    | h::t -> acc h 1 t
;;

assert (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] 
  = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] ) ;;
