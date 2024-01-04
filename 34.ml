(* From p33 *)
let rec gcd a b = if a > b then gcd b a else if a = 0 then b else gcd (b mod a) a ;;
let coprime a b = (gcd a b) = 1 ;;

let phi n = 
  let rec count acc c = 
    if c > n then acc
    else if (gcd n c)=1 then count (acc + 1) (c+1) 
    else count acc (c+1)
  in count 0 1 ;; 

assert (phi 10 = 4);;
assert (phi 13 = 12);;

