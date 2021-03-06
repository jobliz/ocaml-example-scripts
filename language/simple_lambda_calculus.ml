(* 
From the paper "Applications of Generalized Algebraic Data Types in OCaml"

TODO: The other operators.

See:
  https://stackoverflow.com/questions/48049472/how-to-implement-lambda-calculus-in-ocaml
  https://stackoverflow.com/questions/7550200/ocaml-using-a-comparison-operator-passed-into-a-function
  
*)

type typ = 
  | Boolean 
  | Integer
  | Arrow of typ * typ

type exp =
  | And of exp * exp
  | Add of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | App of exp * exp
  | Lam of string * typ * exp
  | Var of string
  | Int of int
  | Bool of bool

let multiprint x =
  match x with
  | Int i -> print_int i
  | _ -> print_endline "Print type not defined"

let e1 = Add(Int 0, Add(Int 1, Int 2))
let e2 = And(Bool false, Bool true)
let e3 = App(Lam("x", Integer, Add(Var "x", Var "x")), Int 1)
let e4 = Add(Int 1, Bool true) (* error! *)

(* todo: reuse match logic *)
let arithmetic_int_match (v1: exp) (v2: exp) (op) =
  match (v1, v2) with
    | (Int i1, Int i2) -> Int ((op) i1 i2)
    | _ -> failwith "IntegerTypeError"

let rec eval (e: exp) = 
  match e with
    | Int i -> Int i
    | Add (e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in (
          match (v1, v2) with 
          | (Int i1, Int i2) -> Int (i1 + i2)
          | _ -> failwith "IntegerTypeError"
        )
    | Sub (e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in (
          match (v1, v2) with 
          | (Int i1, Int i2) -> Int (i1 - i2)
          | _ -> failwith "IntegerTypeError"
        )
    | Mul (e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in (
          match (v1, v2) with 
          | (Int i1, Int i2) -> Int (i1 * i2)
          | _ -> failwith "IntegerTypeError"
        )
    | _ -> failwith "Not implemented"

let () =
  let e = Mul(Int 2,  Add(Int 5, Sub(Int 2, Int 1))) in
  let v = eval e in 
  multiprint v;
  print_endline "";