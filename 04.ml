let rec length l = match l with 
  | []   -> 0
  | _::t -> 1 + length t
;;

assert (length ["a"; "b"; "c"] = 3);;
assert (length [] = 0);;
