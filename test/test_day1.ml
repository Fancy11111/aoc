open Aoc_2023
open OUnit2

(* let test_v1 _ = assert_equal ~msg:"Day 1 should be 142 is" ~printer:(fun a -> a) "142" @@  *)
(*     (Helper.collect_lines "data/day1-1.test" |> Day1.v1) *)
(* let test_v2 _ = assert_equal ~msg:"Day 2 should be 281 is" ~printer:(fun a -> a) "281" @@  *)
(*     (Helper.collect_lines "data/day1-2.test" |> Day1.v2) *)

let suite =
    Helper.create_day_test_suite (module Day1 : Helper.Day);; 

let () =
    run_test_tt_main suite

