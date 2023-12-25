type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

let rec repeat x a = if x=0 then [] else a::(repeat (x-1) a)
;;

let rec decode = function 
  | []               -> []
  | (One a)::t       -> a::(decode t)
  | (Many (x, a))::t -> (repeat x a) @ (decode t)
;;



assert (decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] 
  = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]) ;;
