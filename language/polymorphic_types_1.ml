(* https://blog.klipse.tech/ocaml/2018/03/16/ocaml-polymorphic-types.html *)

type flower =
  | Rose
  | Tulip

type animal = 
  | Dog
  | Cat

let string_of_animal x = match x with
  | Dog  -> "dog"
  | Cat  -> "cat"

let string_of_flower x = match x with 
  | Rose  -> "rose" 
  | Tulip  -> "tulip"

let string_of_flower_or_animal x =
  match x with
  | `Rose -> "rose"
  | `Tulip -> "tulip"
  | `Dog -> "dog"
  | `Cat -> "cat"

let () =
  let rose = `Rose in 
  let rose_string = string_of_flower_or_animal rose in 
  print_endline rose_string;


