let rec range a b = if a > b then [] else if a = b then [b] else a::(range (a+1) b);;

let is_prime n = 
  if n = 1 then false else
  not (List.exists (fun d -> n mod d = 0) (range 2 (n-1)))
;;

let all_primes low high = 
  let rec primes acc cur = 
    if cur > high then acc 
    else if is_prime cur then primes (cur::acc) (cur+1)
    else primes (acc) (cur+1)
  in List.rev ( primes [] low )
;;

assert (List.length (all_primes 2 7920) = 1000);;
