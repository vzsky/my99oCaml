(* From 35 *)
let flat_factors m =
  let rec factors' acc n d =
    if n = 1 then acc
    else if (n mod d = 0) then factors' (d::acc) (n/d) d
    else factors' acc n (d+1)
  in List.rev( factors' [] m 2 )
;;

(* From 10 *)
let encode l = 
  let rec acc a x = function 
    | []   -> [(a, x)]
    | h::t -> if a=h then acc a (x+1) t else (a, x) :: (acc h 1 t)
  in match l with 
    | []   -> [] 
    | h::t -> acc h 1 t
;;

let factors n = encode (flat_factors n);;

assert (factors 315 = [(3, 2); (5, 1); (7, 1)]);;
