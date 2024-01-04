let rec extract n l = 
  if n=0 then [([], l)] else 
  match l with 
  | []   -> []
  | h::t -> (List.map (fun x -> (h::(fst x), snd x)) (extract (n-1) t))
              @ (List.map (fun x -> (fst x, h::(snd x))) (extract n t))
;;

let group l g = 
  let drop_last l= List.rev (List.tl (List.rev l)) in
  let rec group_aux (l: string list) (g: int list): string list list list =
    match g with
    | []   -> [[[]]]
    | h::t -> List.concat ( List.map (fun x ->
          List.map (fun y -> (fst x)::y) (group_aux (snd x) t)
        ) (extract h l) )
  in List.map drop_last (group_aux l g)
;;

assert ( group ["a"; "b"; "c"; "d"] [2; 1] = 
  [[["a"; "b"]; ["c"]]; [["a"; "b"]; ["d"]]; [["a"; "c"]; ["b"]];
  [["a"; "c"]; ["d"]]; [["a"; "d"]; ["b"]]; [["a"; "d"]; ["c"]];
  [["b"; "c"]; ["a"]]; [["b"; "c"]; ["d"]]; [["b"; "d"]; ["a"]];
  [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]] );;
