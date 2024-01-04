let rec extract n l = 
  if n=0 then [[]] else 
  match l with 
  | []   -> []
  | h::t -> (List.map (fun x -> h::x) (extract (n-1) t)) @ (extract n t)
;; 

assert (extract 2 ["a"; "b"; "c"; "d"] =  
  [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]) ;;
