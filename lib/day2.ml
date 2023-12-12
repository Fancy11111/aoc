open Core
open Angstrom

type color = 
    | Red of int
    | Blue of int
    | Green of int

(* type game = (int * (color list list)) *)

let is_ws = (function 
    | ' ' -> true
    | _ -> false
)

(* let not_ws c = not (is_ws c) *)

let ws = skip_while is_ws

let color_showing = Helper.integer <* ws >>= (fun v -> choice [string "blue" >>| (fun _ -> Blue v); string "red" >>| (fun _ -> Red v); string "green" >>| (fun _ -> Green v)])

let showing =  sep_by (string ", ") color_showing

let showings = sep_by (string "; ") showing

let game = ((string "Game") <* ws) *> Helper.integer <* char ':' <* ws >>= (fun id ->  showings >>| (fun css -> (id, css)))

(* let color_to_string c = match c with *)
(*     | Red v -> Format.sprintf "%d red" v *)
(*     | Blue v -> Format.sprintf "%d blue" v *)
(*     | Green v -> Format.sprintf "%d green" v *)
(**)
(* let colors_to_string cs = List.fold ~init:(color_to_string @@ List.hd_exn cs) ~f:(fun acc c -> acc ^ ", " ^ (color_to_string c)) (List.tl_exn cs) *)
(**)
(* let print_game (id, css) =  *)
(*     Format.printf "Game %d: %s\n" id ( *)
(*         List.fold ~init:(colors_to_string @@ List.hd_exn css) ~f:(fun acc c -> acc ^ "; " ^ colors_to_string c) @@ List.tl_exn css   *)
(*     ) *)

let parse_game line =
    (* let _ = print_endline line in *)
    match parse_string ~consume:Consume.Prefix game line with 
        | Ok(g) -> (
            (* let _ = print_game g in *)
            g
        )
        | Error(e) -> (
            let _ = print_endline ("Error: " ^ e) in 
            (0, [])
        )

let is_valid_color c =
    match c with
        | Red v -> v <= 12
        | Blue v -> v <= 14
        | Green v -> v <= 13

let is_valid_game (_, showings) =
    List.for_all showings ~f:(fun showing -> List.for_all ~f:is_valid_color showing)

let v1 lines = 
    let mapped = List.filter_map ~f:(fun l -> 
        let parsed_game = parse_game l in
        if (is_valid_game parsed_game) then Some(parsed_game) else None
    ) lines in
    (* let _ = List.iter ~f:print_game mapped in *)
    Format.sprintf "%d" @@
    (List.fold mapped ~init:0 ~f:(fun acc (id,_) -> acc+id))

let max_in_game (_, css) = 
    let flat = List.fold ~init:[] ~f:(fun acc cs -> List.append acc cs) css in
    List.fold ~init:(0, 0, 0) ~f:(fun (r,g,b) c -> match c with
        | Red v -> ((max v r), g, b)
        | Green v -> (r, (max v g), b)
        | Blue v -> (r, g, (max v b))
    ) flat

let v2 lines = 
    let mapped = List.map ~f:parse_game lines in
    (* let _ = List.iter ~f:print_game mapped in *)
    let res = List.fold ~init:0 ~f:(fun acc g -> 
        let (r,g,b) = max_in_game g in
        acc + (r * g * b)) mapped in
    Format.sprintf "%d" @@
    res
