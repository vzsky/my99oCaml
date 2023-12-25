(* p22 *)
let range x y = 
  let rec r x y = if x=y then [x] else x::(r (x+1) y) in
  if x > y then List.rev (r y x) else r x y
;; 

(* p23 *)
let rand_select = 
  let rec get x n = function 
    | []   -> []
    | h::t -> if n=0 then h :: (x @ t) else get (h::x) (n-1) t
  in 
  let rand n l = get [] (Random.int n) l
  in 
  let rec select l n = 
    if n>0 then 
      let res :: rest = rand (List.length l) l in
      res :: select rest (n-1)
    else []
  in select
;;  

(* work *)
let lotto_select x y = rand_select (range 1 y) x ;;

(* No Assertion *)
