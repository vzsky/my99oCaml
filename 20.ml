let remove_at m l = 
  let rec acc x n = function 
    | []   -> x 
    | h::t -> if n=0 then acc x (n-1) t else acc (h::x) (n-1) t
  in List.rev (acc [] m l)
;;

let rec remove_at' m = function 
  | []   -> []
  | h::t -> if m=0 then t else h::(remove_at' (m-1) t)
;;

assert (remove_at  1 ["a"; "b"; "c"; "d"] = ["a"; "c"; "d"]);;
assert (remove_at' 1 ["a"; "b"; "c"; "d"] = ["a"; "c"; "d"]);;
