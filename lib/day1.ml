let is_digit = function '0' .. '9' -> true | _ -> false;;

let digit_char_to_int c = int_of_char c  - (int_of_char '0')

let concat_if_digit s d = 
    if is_digit d then (s @ [(String.make 1 d)]) else s;;

(* let number_if_digit c = *)
(*     if is_digit c then Some (digit_char_to_int c) else None *)

let remove_all_but_digit s = 
    String.fold_left concat_if_digit [] s;;

(* let getCharAsString s n = *)
(*     String.make 1 (String.get s n) *)

(* let getFirstAndLast s = *)
(*     (String.cat (getCharAsString s 0) (getCharAsString s (String.length s - 1)));; *)

let digit_words = [
    "zero", 0;
    "one", 1;
    "two", 2;
    "three", 3;
    "four", 4;
    "five", 5;
    "six", 6;
    "seven", 7;
    "eight", 8;
    "nine", 9;
];;

let is_digit_or_digit_word str ind = 
    let c = str.[ind] in
    if c |> is_digit then Some(digit_char_to_int c)
    else 
        (List.find_map (fun (word, value) -> 
            match Core.String.substr_index ~pos:ind ~pattern:word str with
            | Some(pos) -> if pos = ind then Some(value) else None
            | None -> None
        ) digit_words) 


let v1 lines =
    Format.sprintf "%d" 
   (List.fold_left (fun acc c -> (acc + c)) 0 
    (List.map (fun s -> int_of_string ((List.hd s) ^ (List.hd (List.rev s)) ))
    (List.map remove_all_but_digit lines)
    ));;

let v2 lines =
    Format.sprintf "%d" @@
    List.fold_left (fun acc c -> (acc + c)) 0 @@
    List.map (fun s -> 
        let mapped = List.filter_map  (fun ind -> is_digit_or_digit_word s ind) (Core.List.range ~start:`inclusive ~stop:`exclusive 0 (String.length s)) in 
        ((List.hd mapped) * 10) + (List.hd (List.rev mapped))
    ) lines

let d = 1
