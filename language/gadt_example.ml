(* https://mads-hartmann.com/ocaml/2015/01/05/gadt-ocaml.html *)

open Printf

type value =
  | Bool of bool
  | Int of int

type expr =
  | Value of value
  | If of expr * expr * expr
  | Eq of expr * expr
  | Lt of expr * expr

let rec eval: expr -> value = function
  | Value v -> v
  | Lt (x, y) -> 
    begin match eval x, eval y with
      | Int x, Int y -> Bool (x < y)
      | Int _, Bool _
      | Bool _, Int _
      | Bool _, Bool _ -> failwith "Invalid AST"
    end
  | If (b, l, r) -> 
    begin match eval b with
      | Bool true -> eval l
      | Bool false -> eval r
      | Int _ -> failwith "Invalid AST"
    end
  | Eq (a, b) -> 
    begin match eval a, eval b with
      | Int  x, Int  y -> Bool (x = y)
      | Bool _, Bool _
      | Bool _, Int  _
      | Int  _, Bool _ -> failwith "Invalid AST"
    end

let () = 
  let valid = (If ((Lt ((Value (Int 2)), (Value (Int 4)))),
          (Value (Int 42)),
          (Value (Int 0)))) in
  let invalid = (Eq ((Value (Int 42)), (Value (Bool false)))) in
  printf "what";
