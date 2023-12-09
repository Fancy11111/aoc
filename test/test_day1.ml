open Aoc_2023
open OUnit2

let test_v1 _ = assert_equal ~msg:"Day 1 should " "281" @@ (Helper.collect_lines "data/day1-1.test" |> Day1.v1)

let suite =
    "Test Day 1" >::: [
        "v1" >:: test_v1;
    ]

let () =
    run_test_tt_main suite

(* let () = *)
(*     assert_equal ~msg:"Day 1 should" "281" @@ (Helper.collect_lines "../../../test/day1-2.test" |> Day1.v2) *)
