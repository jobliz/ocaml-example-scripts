(* http://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/data/parameterized_variants.html *)

open Printf

type 'a mylist = Nil | Cons of 'a * 'a mylist

let lst3 = Cons (3, Nil)  (* similar to [3] *)
let lst_hi = Cons ("hi", Nil)  (* similar to ["hi"] *)

let rec length : 'a mylist -> int = function
  | Nil -> 0
  | Cons (_,t) -> 1 + length t

let empty : 'a mylist -> bool = function
  | Nil -> true
  | Cons _ -> false

let () =
    let test = Cons (3, Cons(5, Nil)) in
    print_int(length test);
