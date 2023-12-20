open Aoc_2023
open OUnit2

let suite =
   Helper.create_day_test_suite (module Day2 : Helper.Day) 

let () =
    run_test_tt_main suite

(* let () = *)
(*     assert_equal ~msg:"Day 1 should" "281" @@ (Helper.collect_lines "../../../test/day1-2.test" |> Day1.v2) *)
