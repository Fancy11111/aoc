val collect_lines : string -> string list 
val integer : int Angstrom.t
val is_ws : char -> bool 
val ws' : string Angstrom.t

type version =
    | V1
    | V2

module type Day = sig
  val d : int
  val v1 : string list -> string
  val v2 : string list -> string
end

val create_day_test_suite : (module Day) -> OUnit2.test 

