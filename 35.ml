let factors m = 
  let rec factors' acc n d = 
    if n = 1 then acc 
    else if (n mod d = 0) then factors' (d::acc) (n/d) d
    else factors' acc n (d+1)
  in List.rev ( factors' [] m 2 )
;;

assert (factors 315 = [3; 3; 5; 7]);;
