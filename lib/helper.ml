open Angstrom;;
open OUnit2;;

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let is_ws = function 
    | ' ' -> true
    | _ -> false

let ws' = take_while (is_ws)


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

type version =
    | V1
    | V2

module type Day = sig
    val d : int
    val v1 : string list -> string
    val v2 : string list -> string
end

let create_test_message d vn exp = Format.sprintf "Day %d.%d should be %s" d vn exp 

let create_day_test expected msg actual _ = OUnit2.assert_equal ~msg ~printer:(fun a -> a) expected actual 

let create_day_test_suite (module M: Day) = 
    let d = M.d in
    let get_test_data = fun vn -> collect_lines (Format.sprintf "data/day%d-%d.test" d vn) in
    let (td_1, td_2) = (get_test_data 1, get_test_data 2) in
    let (expected_1, expected_2) = (List.hd td_1, List.hd td_2) in 
    let (lines_1, lines_2) = (List.tl td_1, List.tl td_2) in
    let (msg_1, msg_2) = (create_test_message d 1 expected_1, create_test_message d 2 expected_2) in
    let (res_1, res_2) = (M.v1 lines_1, M.v2 lines_2) in
    let (t1, t2) = (create_day_test expected_1 msg_1 res_1, create_day_test expected_2 msg_2 res_2 ) in
    (
        (Format.sprintf "Test Day %d" d) >::: [
            "v1" >:: t1;
            "v2" >:: t2;
        ]
    )
(*     assert_equal ~msg:msg ~printer:(fun a -> a) expected @@ match v with  *)
(*         | V1 -> M.v1 lines *)
(*         | V2 -> M.v1 lines *)
(**)
(* let test_v1 _ = assert_equal ~msg:"Day 3.1 should be 4361" ~printer:(fun a -> a) "4361" @@  *)
(*     (Helper.collect_lines "data/day3-1.test" |> Day3.v1) *)
(* let test_v2 _ = assert_equal ~msg:"Day 3.2 should be 467835 is" ~printer:(fun a -> a) "467835" @@  *)
(*     (Helper.collect_lines "data/day3-2.test" |> Day3.v1) *)

(* let suite = *)
(*     "Test Day 4" >::: [ *)
(*         "v1" >:: test_v1; *)
(*         (* "v2" >:: test_v2; *) *)
(*     ] *)

