(* From 40 *)
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

let rec goldbach_list low high =
  if low mod 2 = 1 then goldbach_list (low+1) high 
  else let rec get_list acc cur = 
    if cur > high then acc 
    else get_list ((cur, (goldbach cur))::acc) (cur+2)
    in List.rev ( get_list [] low )
;;

let goldbach_limit low high limit = 
  List.filter (fun (n, (a, b)) -> a > limit) (goldbach_list low high)
;;

assert (goldbach_list 9 20 = 
  [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
    (20, (3, 17))] ) ;;

assert (goldbach_limit 1 2000 50 = 
  [(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
    (1928, (61, 1867))] );;
