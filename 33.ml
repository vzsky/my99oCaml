(* From 32 *)
let rec gcd a b = if a > b then gcd b a else if a = 0 then b else gcd (b mod a) a ;;
let coprime a b = (gcd a b) = 1 ;;

assert (coprime 13 27);;
assert (not (coprime 202536 7826));;
