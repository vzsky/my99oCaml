let opr = ref 0;;
let domod a b = incr opr; a mod b ;;

(* From 34 *)
let rec gcd a b = if a > b then gcd b a else if a = 0 then b else gcd (domod b a) a ;;
let coprime a b = (gcd a b) = 1 ;;

let phi n = 
  opr := 0;
  let rec count acc c = 
    if c > n then acc
    else if (gcd n c)=1 then count (acc + 1) (c+1) 
    else count acc (c+1)
  in count 0 1 ;; 

(* From 37*)
let flat_factors m =
  let rec factors' acc n d =
    if n = 1 then acc
    else if (domod n d = 0) then factors' (d::acc) (n/d) d
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
  if domod a 2 == 0 then h*h 
  else h*h*a
;;

let phi_prime p m = (pow p (m-1)) * (p-1);;
let phi_improved n = opr := 0; List.fold_left (fun acc a -> acc * a) 1 (List.map (fun x -> phi_prime (fst x) (snd x)) (factors n)) ;;

(* Compare count of mod *)
assert (phi 10090 = 4032);; 
assert (!opr = 77394);;
assert (phi_improved 10090 = 4032);;
assert (!opr = 1010);;
