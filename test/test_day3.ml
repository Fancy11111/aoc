
open Aoc_2023
open OUnit2

let suite =
   Helper.create_day_test_suite (module Day3 : Helper.Day)


let () =
    run_test_tt_main suite

