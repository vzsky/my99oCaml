let range x y = 
  let rec r x y = if x=y then [x] else x::(r (x+1) y) in
  if x > y then List.rev (r y x) else r x y
;; 
    
assert (range 4 9 = [4; 5; 6; 7; 8; 9]) ;;
assert (range 9 4 = [9; 8; 7; 6; 5; 4]) ;;
