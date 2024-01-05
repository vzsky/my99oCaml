type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

let table2 a b e = 
  let rec eval u v e = 
    match e with 
    | And (x, y) -> eval u v x && eval u v y
    | Or  (x, y) -> eval u v x || eval u v y
    | Not x      -> not (eval u v x) 
    | Var s      -> if s = a then u else v 
  in List.map (fun (u, v) -> (u, v, eval u v e)) [(true, true); (true, false); (false, true); (false, false)] 
;;

assert (table2 "a" "b" (And (Var "a", Or (Var "a", Var "b"))) = 
  [(true, true, true); (true, false, true); (false, true, false);
    (false, false, false)]);;
