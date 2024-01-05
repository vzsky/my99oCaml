let rec gray n = 
  if n = 1 then ["0"; "1"]
  else let g = gray (n-1) in
    (List.map (fun x -> "0"^x) g) @ (List.map (fun x -> "1"^x) (List.rev g))
;;

assert (gray 1 = ["0"; "1"]);;
assert (gray 2 = ["00"; "01"; "11"; "10"]);;
assert (gray 3 = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]);;
