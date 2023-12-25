let pack l =
  let rec acc a x = function
    | []   -> [x]
    | h::t -> if a=h then acc a (h::x) t else x :: acc h [h] t
  in match l with 
    | []   -> []
    | f::r -> acc f [f] r
;;

assert (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] 
  = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]] ) ;;
