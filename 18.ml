let slice l x y= 
  let rec acc r x y = function 
    | []   -> r
    | h::t -> if x<=0 && y>=0 then acc (h::r) (x-1) (y-1) t else acc r (x-1) (y-1) t
  in List.rev (acc [] x y l)
;;

assert (slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 = ["c"; "d"; "e"; "f"; "g"]);;
