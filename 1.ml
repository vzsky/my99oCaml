let rec last l = match l with
                | []   -> None 
                | [e]  -> Some e
                | _::t -> last t ;; 

assert (last ["a" ; "b" ; "c" ; "d"] = Some "d") ;;
assert (last [] = None) ;;
