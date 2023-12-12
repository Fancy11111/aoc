open Core;;
open Angstrom;;

let number_list = sep_by1  (char ' ') Helper.integer

let numbers = number_list >>= (fun ns -> 
    string " | " *> number_list >>|  (fun _ns -> (ns,_ns))
)

let game = string "Game" *> Helper.integer *> string ": " *> numbers

let parse_game line =
    match parse_string ~consume:Consume.Prefix game line with 
        | Ok(g) -> g
        | Error(_) -> ([],[])

let game_score (w_ns, m_ns) = 
    let found = (List.fold w_ns ~init:(0-1) ~f:(fun acc c ->
        if List.exists m_ns ~f:(fun n -> c = n) then acc+1
        else acc
    )) in
    if found = - 1 then 0
    else Int.pow 2 found

let v1 lines =
    let games = List.map ~f:(fun l -> parse_game l) lines in
    let scores = List.fold games ~init:0 ~f:(fun acc g -> acc + (game_score g)) in
    Format.sprintf "%d\n" scores
