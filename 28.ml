let rec sort cmp = 
  let rec least (l:'a list list): 'a list list = match l with
    | []   -> []
    | h::t -> match (least t) with 
      | []   -> [h]
      | a::b -> (cmp h a)@b
  in fun (lst: 'a list list) ->  match lst with 
  | []   -> []
  | l -> match (least l) with 
    | []   -> []
    | h::t -> h::(sort cmp t)
;;

let cmp_length a b = if (List.length a <= List.length b) then [a;b] else [b;a] ;;
let length_sort = sort cmp_length ;;

let frequency_sort = function 
  | [] -> []
  | l  -> let count (n:int) = List.length ( List.filter (fun x -> x=n) (List.map (List.length) l) )
    in sort (fun a b -> if (count (List.length a) <= count (List.length b)) then [a;b] else [b;a]) l
;;

assert (length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
  ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]] 
  = [["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"];
    ["i"; "j"; "k"; "l"]] );;
assert (frequency_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
    ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]]
  = [["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"];
    ["d"; "e"]; ["m"; "n"]] );;
