let is_digit = function '0' .. '9' -> true | _ -> false;;

let concatIfDigit s d = 
    if is_digit d then (s @ [(String.make 1 d)]) else s;;

let removeAllButDigit s = 
    String.fold_left concatIfDigit [] s;;

(* let getCharAsString s n = *)
(*     String.make 1 (String.get s n) *)

(* let getFirstAndLast s = *)
(*     (String.cat (getCharAsString s 0) (getCharAsString s (String.length s - 1)));; *)

let v1 lines =
    Format.sprintf "%d" 
   (List.fold_left (fun acc c -> (acc + c)) 0 
    (List.map (fun s -> int_of_string ((List.hd s) ^ (List.hd (List.rev s)) ))
    (List.map removeAllButDigit lines)
    ));;

let v2 = v1
