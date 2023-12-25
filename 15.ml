let rec replicate l n = 
  let rec repeat a n = if n=0 then [] else a::(repeat a (n-1))
  in match l with 
     | []   -> []
     | h::t -> (repeat h n) @ (replicate t n)
;;

let replicate' l' n' = 
  let rec acc x n = function 
    | []   -> x
    | h::t -> if n=0 then acc x n' t else acc (h::x) (n-1) (h::t)
  in List.rev (acc [] n' l')
;;

assert ( replicate  ["a"; "b"; "c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] );;
assert ( replicate' ["a"; "b"; "c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] );;
