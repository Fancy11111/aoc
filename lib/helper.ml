open Angstrom;;

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string


let collect_lines input = 
    let lines = ref [] in
    let chan = open_in input in
    try
        while true; do
            lines := input_line chan :: !lines
        done; !lines
    with End_of_file ->
        close_in chan;
        List.rev !lines;;
