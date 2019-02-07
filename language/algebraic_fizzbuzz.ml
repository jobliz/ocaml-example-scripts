(*
Comparing imperative and ADT implementations of FizzBuzz.

Original idea and implementation in Scala:
  http://engineering.monsanto.com/2016/01/11/algebraic-data-types/
  https://github.com/MonsantoCo/algebraic-datatypes

Consider reading:
  https://www.reddit.com/r/ocaml/comments/6rkeu9/fizzbuzz/
*)

let imperative_fizzbuzz n =
  if n mod 3 = 0 && n mod 5 = 0 then "FizzBuzz "
  else if n mod 3 = 0 then "Fizz "
  else if n mod 5 = 0 then "Buzz "
  else "JustInt"

let is_mod m i = 
  i mod m = 0

let is_mod_3 = is_mod 3

let is_mod_5 = is_mod 5

(* [f]izz[b]uzz [r]esult *)
type fbr =
  | Fizz of int
  | Buzz of int
  | FizzBuzz of int
  | JustInt of int

let typed_fizzbuzz (n: int): fbr =
  if is_mod_3 n && is_mod_5 n then FizzBuzz n
  else if is_mod_3 n then Fizz n
  else if is_mod_5 n then Buzz n
  else JustInt n

let fizzbuzz_repr (n: fbr): string = 
  match n with
    | Fizz i -> "Fizz "
    | Buzz i -> "Buzz "
    | FizzBuzz i -> "FizzBuzz "
    | JustInt i -> "JustInt "

let () = 
  3 |> imperative_fizzbuzz |> print_bytes;
  5 |> imperative_fizzbuzz |> print_bytes;
  15 |> imperative_fizzbuzz |> print_bytes;
  7 |> imperative_fizzbuzz |> print_bytes;
  
  print_endline "";
  print_endline "";
  
  3 |> typed_fizzbuzz |> fizzbuzz_repr |> print_bytes;
  5 |> typed_fizzbuzz |> fizzbuzz_repr |> print_bytes;
  15 |> typed_fizzbuzz |> fizzbuzz_repr |> print_bytes;
  7 |> typed_fizzbuzz |> fizzbuzz_repr |> print_bytes;