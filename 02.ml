let rec last_two = function
  | []    -> None
  | [_]   -> None
  | [a;b] -> Some (a, b)
  | _::t  -> last_two t
;;

assert (last_two ["a"; "b"; "c"; "d"] = Some ("c", "d")) ;;
assert (last_two ["a"] = None) ;;
