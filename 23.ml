let rand_select= 
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

(* No Assertions *)
