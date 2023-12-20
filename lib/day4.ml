open Core;;
open Angstrom;;

let number_list = sep_by1 Helper.ws' Helper.integer

let numbers = number_list >>= (fun ns -> 
    Helper.ws' *> char '|' *> Helper.ws' *> number_list >>|  (fun _ns -> (ns,_ns))
)

let game = string "Card" *> Helper.ws' *> Helper.integer *> string ": " *> Helper.ws' *> numbers

let parse_game line =
    match parse_string ~consume:Consume.Prefix game line with 
        | Ok(g) -> g
        | Error(_) -> ([],[])

let matching_nums (w_ns, m_ns) = 
    (List.fold w_ns ~init:(0) ~f:(fun acc c ->
        let matches = List.exists m_ns ~f:(fun n -> c = n) in 
        if matches then acc+1
        else acc
    ))

let game_score (w_ns, m_ns) = 
    let found = matching_nums (w_ns, m_ns) - 1 in
    if found = (0 - 1) then 0
    else Int.pow 2 found

let v1 lines =
    let games = List.map ~f:(fun l -> parse_game l) lines in
    let scores = List.fold games ~init:0 ~f:(fun acc g -> acc + (game_score g)) in
    Format.sprintf "%d" scores

let rec add_to_games games i n =
    if i < 1 then games else 
    match games with 
    | [] -> []
    | (s, g)::gs -> (s+n,g)::(add_to_games gs (i-1) n)

let rec calc_game_score games =
    match games with 
    | [] -> []
    | (s, g)::gs -> 
        let v = matching_nums g in 
        (s, g)::(calc_game_score (add_to_games gs v s))

let solve_v2 lines =
    let games = List.map ~f:(fun l -> (1, parse_game l)) lines in
    let mapped_games = calc_game_score games in 
    let score = List.fold mapped_games ~init:0 ~f:(fun acc (s,_) -> s + acc) in
    Format.sprintf "%d" score

let v2 = solve_v2;;

let d = 4
