(* 
From the paper "Applications of Generalized Algebraic Data Types in OCaml"

TODO: The other operators.
*)

type typ = 
  | Boolean 
  | Integer 
  | Arrow of typ * typ

type exp =
  | And of exp * exp
  | Add of exp * exp
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

let rec eval e = 
  match e with
    | Int i -> Int i
    | Add (e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in (
          match (v1, v2) with 
          | (Int i1, Int i2) -> Int (i1 + i2)
          | _ -> failwith "IntegerTypeError"
        )
    | _ -> failwith "Not implemented"

let () =
  let v1 = eval e1 in 
  multiprint v1