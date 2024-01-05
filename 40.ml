(* From 31 *)
let rec range a b = if a > b then [] else if a = b then [b] else a::(range (a+1) b);;

let is_prime n = 
  if n = 1 then false else
  not (List.exists (fun d -> n mod d = 0) (range 2 (n-1)))
;;

let goldbach n = 
  let rec try_prime d = 
    if (is_prime (n-d)) && (is_prime d) then (d, n-d) else try_prime (d+1);
  in try_prime 2
;;

assert (goldbach 28 = (5, 23));;
