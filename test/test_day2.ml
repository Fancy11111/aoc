open Aoc_2023
open OUnit2

let test_v1 _ = assert_equal ~msg:"Day 2.1 should be 8" ~printer:(fun a -> a) "8" @@ 
    (Helper.collect_lines "data/day2-1.test" |> Day2.v1)
let test_v2 _ = assert_equal ~msg:"Day 2 should be 2286 is" ~printer:(fun a -> a) "2286" @@ 
    (Helper.collect_lines "data/day2-2.test" |> Day2.v2)

let suite =
    "Test Day 1" >::: [
        "v1" >:: test_v1;
        "v2" >:: test_v2;
    ]

let () =
    run_test_tt_main suite

(* let () = *)
(*     assert_equal ~msg:"Day 1 should" "281" @@ (Helper.collect_lines "../../../test/day1-2.test" |> Day1.v2) *)
