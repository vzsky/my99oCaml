let rec range a b = if a > b then [] else if a = b then [b] else a::(range (a+1) b);;

let is_prime n = 
  if n = 1 then false else
  not (List.exists (fun d -> n mod d = 0) (range 2 (n-1)))
;;

assert( not (is_prime 1) );;
assert( is_prime 7 );;
assert( not (is_prime 12) );;
