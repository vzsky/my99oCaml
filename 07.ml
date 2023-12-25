type 'a node =
  | One of 'a 
  | Many of 'a node list
;;

let rec flatten n = match n with 
  | []          -> []
  | One a :: t  -> a :: flatten t
  | Many l :: t -> flatten l @ flatten t
;;

let flatten' n =
  let rec acc x = function 
    | []          -> x
    | One a :: t  -> acc (a::x) t
    | Many l :: t -> acc (acc x l) t
  in List.rev (acc [] n)
;;

assert (flatten  [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] = ["a"; "b"; "c"; "d"; "e"]) ;;
assert (flatten' [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] = ["a"; "b"; "c"; "d"; "e"]) ;;
