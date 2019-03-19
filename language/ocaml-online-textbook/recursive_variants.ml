(* http://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/data/recursive_variants.html *)

type intlist = Nil | Cons of int * intlist

let lst3 = Cons (3, Nil)  (* similar to 3::[] or [3]*)
let lst123 = Cons(1, Cons(2, lst3)) (* similar to [1;2;3] *)

let rec sum (l:intlist) : int=
  match l with
  | Nil -> 0
  | Cons(h,t) -> h + sum t

let rec length : intlist -> int = function
  | Nil -> 0
  | Cons (_,t) -> 1 + length t

let empty : intlist -> bool = function
  | Nil -> true
  | Cons _ -> false