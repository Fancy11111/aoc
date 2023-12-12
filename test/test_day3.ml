
open Aoc_2023
open OUnit2

let test_v1 _ = assert_equal ~msg:"Day 3.1 should be 4361" ~printer:(fun a -> a) "4361" @@ 
    (Helper.collect_lines "data/day3-1.test" |> Day3.v1)
let test_v2 _ = assert_equal ~msg:"Day 2 should be 467835 is" ~printer:(fun a -> a) "467835" @@ 
    (Helper.collect_lines "data/day3-2.test" |> Day3.v1)

let suite =
    "Test Day 3" >::: [
        "v1" >:: test_v1;
        (* "v2" >:: test_v2; *)
    ]

let () =
    run_test_tt_main suite

(* let () = *)
(*     assert_equal ~msg:"Day 1 should" "281" @@ (Helper.collect_lines "../../../test/day1-2.test" |> Day1.v2) *)
