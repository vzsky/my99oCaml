let rec gcd a b = if a > b then gcd b a else if a = 0 then b else gcd (b mod a) a ;;

assert (gcd 13 27 = 1);;
assert (gcd 20536 7826 = 2);;
