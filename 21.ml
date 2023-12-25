let rec insert_at s n = function 
  | []   -> [s]
  | h::t -> if n=0 then s::(h::t) else h::(insert_at s (n-1) t)
;;

assert (insert_at "alfa" 1 ["a"; "b"; "c"; "d"] = ["a"; "alfa"; "b"; "c"; "d"]) ;;
assert (insert_at "alfa" 3 ["a"; "b"; "c"; "d"] = ["a"; "b"; "c"; "alfa"; "d"]) ;;
assert (insert_at "alfa" 4 ["a"; "b"; "c"; "d"] = ["a"; "b"; "c"; "d"; "alfa"]) ;;
