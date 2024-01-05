(* From 36 *)
let flat_factors m =
  let rec factors' acc n d =
    if n = 1 then acc
    else if (n mod d = 0) then factors' (d::acc) (n/d) d
    else factors' acc n (d+1)
  in List.rev( factors' [] m 2 )
;;

let encode l = 
  let rec acc a x = function 
    | []   -> [(a, x)]
    | h::t -> if a=h then acc a (x+1) t else (a, x) :: (acc h 1 t)
  in match l with 
    | []   -> [] 
    | h::t -> acc h 1 t
;;

let factors n = encode (flat_factors n);;

let rec pow a b = 
  if b = 0 then 1 else
  let h = pow a (b/2) in 
  if a mod 2 == 0 then h*h 
  else h*h*a
;;

let phi_prime p m = (pow p (m-1)) * (p-1);;
let phi_improved n = List.fold_left (fun acc a -> acc * a) 1 (List.map (fun x -> phi_prime (fst x) (snd x)) (factors n)) ;;

assert (phi_improved 10 = 4);;
assert (phi_improved 13 = 12);;

