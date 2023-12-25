let rec rev l = match l with 
  | []   -> []
  | h::t -> rev t @ [h]
;;

let rev' = 
  let rec acc l = function
    | []   -> l
    | h::t -> acc (h::l) t
  in acc []
;;

assert (rev  ["a"; "b"; "c"] = ["c"; "b"; "a"]);;
assert (rev' ["a"; "b"; "c"] = ["c"; "b"; "a"]);;
