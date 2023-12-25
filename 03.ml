let rec at n l = match l with 
  | []   -> None 
  | h::t -> match n with 
            | 0 -> None 
            | 1 -> Some h 
            | _ -> at (n-1) t
;;

assert (at 3 ["a"; "b"; "c"; "d"; "e"] = Some "c") ;;
assert (at 3 ["a"] = None) ;;
