type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

let table vn e = 
  let rec get vs v :bool = 
    match vs with 
    | []   -> false (* This should never be reached *) 
    | h::t -> if v = Var (fst h) then (snd h) else get t v 
  in 
  let rec variable vn = 
    match vn with 
    | []   -> [[]]
    | h::t -> (List.map (fun x -> (h, true)::x) (variable t)) @ (List.map (fun x -> (h, false)::x) (variable t))
  in 
  let rec eval vs e : bool = 
    match e with 
    | And (x, y) -> eval vs x && eval vs y
    | Or  (x, y) -> eval vs x || eval vs y
    | Not x      -> not (eval vs x) 
    | Var s      -> get vs e
  in List.map (fun vs -> (vs, eval vs e)) (variable vn)
;;

assert( let a = Var "a" and b = Var "b" and c = Var "c" in
  table ["a"; "b"; "c"] (Or (And (a, Or (b,c)), Or (And (a,b), And (a,c)))) = 
  [([("a", true); ("b", true); ("c", true)], true);
    ([("a", true); ("b", true); ("c", false)], true);
    ([("a", true); ("b", false); ("c", true)], true);
    ([("a", true); ("b", false); ("c", false)], false);
    ([("a", false); ("b", true); ("c", true)], false);
    ([("a", false); ("b", true); ("c", false)], false);
    ([("a", false); ("b", false); ("c", true)], false);
    ([("a", false); ("b", false); ("c", false)], false)]);;
