open Printf

type 'a tree =
  | Br of 'a * 'a tree * 'a tree
  | Lf

(* size : α tree → int *)
let rec size tr =
  match tr with
  | Br (_, l, r) -> 1 + size l + size r
  | Lf -> 0

(* total : int tree → int *)
(* adds all integers on the tree *)
let rec total tr =
  match tr with
  | Br (x, l, r) -> x + total l + total r
  | Lf -> 0

let max x y =
  if x > y then x else y

let rec maxdepth tr =
  match tr with
  | Br (_, l, r) -> 1 + max (maxdepth l) (maxdepth r)
  | Lf -> 0

(* p. 94 *)

(* From the book: 
Lookup is simple: start at the top, and if we have not found the key we 
are looking for, go left or right depending upon whether the required key 
is smaller or larger than the value at the current branch. If we reach a leaf,
the key was not in the tree (assuming the tree is a well-formed binary search 
tree), and we raise an exception. *)

let rec lookup tr k =
  match tr with
  | Lf -> None
  | Br ((k', v), l, r) ->
    if k = k' then Some v  (* found the key, return the value *)
    else if k < k' then lookup l k (* go left *)
    else lookup r k (* go right *)

let () =  
  let t2 = Br (2, Br (1, Lf, Lf), Lf) in
  print_int (size t2);
  print_endline "";
  print_int (total t2);
  print_endline "";
  print_int (maxdepth t2);
  print_endline "";
