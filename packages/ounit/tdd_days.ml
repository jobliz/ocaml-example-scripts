(*
http://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/data/tdd.html 
ocamlbuild tdd_days.ml tdd_days.native -pkgs oUnit && ./tdd_days.native

Process. Let's review the process of TDD:

1. Write a failing unit test case. Run the test suite to prove that the test case fails.
2. Implement just enough functionality to make the test case pass. Run the test suite to 
   prove that the test case passes.
3. Improve code as needed. In the example above we refactored the test suite, but often 
   we'll need to refactor the functionality being implemented.
4. Repeat until you are satisfied that the test suite provides evidence that your 
   implementation is correct.
*)

open OUnit2

type day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

(* let next_weekday d = failwith "Unimplemented" *)

let next_weekday d = 
  match d with 
  | Monday -> Tuesday
  | Tuesday -> Wednesday
  | Wednesday -> Thursday
  | Thursday -> Friday
  | Friday -> Monday
  | Saturday -> Monday
  | Sunday -> Monday

let make_next_weekday_test name expected_output input= 
  name >:: (fun _ -> assert_equal expected_output (next_weekday input))

(* 
let tests = "test suite for next_weekday" >::: [
  "tue_after_mon"  >:: (fun _ -> assert_equal (next_weekday Monday) Tuesday);
]
*)

let tests = "test suite for next_weekday" >::: [
  make_next_weekday_test "tue_after_mon" Tuesday Monday;
  make_next_weekday_test "wed_after_tue" Wednesday Tuesday;
  make_next_weekday_test "thu_after_wed" Thursday Wednesday;
  make_next_weekday_test "fri_after_thu" Friday Thursday;
  make_next_weekday_test "mon_after_fri" Monday Friday;
  make_next_weekday_test "mon_after_sat" Monday Saturday;
  make_next_weekday_test "mon_after_sun" Monday Sunday;
]

let _ = run_test_tt_main tests