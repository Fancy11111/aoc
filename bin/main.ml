open Aoc_2023

let () = print_endline "Hello, World!"

let usage_msg = "aoc_2023 [-d day] <file1>"

let input_file = ref ""

let day = ref 2 

let anon_fun filename = 
    input_file := filename

let speclist =
    [("-d", Arg.Set_int day, "Set the day to start")]

let () =
    Arg.parse speclist anon_fun usage_msg;

input_file :=
    if String.length input_file.contents  == 0 
        then Format.sprintf "inputs/day%d.input" day.contents 
        else input_file.contents

let get_day d = 
    match d with
    | 1 -> ((Some Day1.v1), (Some Day1.v2)) 
    | 2 -> ((Some Day2.v1), (Some Day2.v2)) 
    | _ -> (None, None) 

(* let () = *)
     
    (* List.iter (fun in_file ->  *)
        (* (let lines = Aoc_2023.Helper.collect_lines in_file in *)
        
    (* ) *)

let () =
    let (v1, v2) = get_day day.contents in
    let lines = Aoc_2023.Helper.collect_lines input_file.contents in
    let () = match v1 with
        | Some(f) -> print_endline @@ (f lines)
        | None -> print_newline ()
    in
    match v2 with
        | Some(f) -> print_endline @@ f lines
        | None -> print_newline ()
