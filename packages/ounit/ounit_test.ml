(* ocamlbuild ounit_test.ml ounit_test.native -pkgs oUnit *)

open OUnit2

let rec sum = function
  | []    -> 0
  | x::xs -> x + sum xs

let make_sum_test name expected_output input =
  name >:: (fun _ -> assert_equal expected_output (sum input) ~printer:string_of_int)

(* 
let tests = "test suite for sum" >::: [
  "empty"  >:: (fun _ -> assert_equal 0 (sum []));
  "one"    >:: (fun _ -> assert_equal 1 (sum [1]));
  "onetwo" >:: (fun _ -> assert_equal 3 (sum [1; 2]));
]
*)

let tests = "test suite for sum" >::: [
  make_sum_test "empty" 0 [];
  make_sum_test "one" 1 [1];
  make_sum_test "onetwo" 3 [1; 2];
]

let _ = run_test_tt_main tests