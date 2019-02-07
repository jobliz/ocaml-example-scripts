(*
The angstrom calculator example exposed to the command line through the arg module.

Compile with:
  ocamlbuild angstrom_calc.ml angstrom_calc.native -pkg angstrom

Execute as:
  ./angstrom_calc.native "(2+3)*2"

See:
  https://github.com/inhabitedtype/angstrom
  https://stackoverflow.com/questions/16950687/integer-exponentiation-in-ocaml
  http://blog.shaynefletcher.org/2016/09/custom-operators-in-ocaml.html
  https://www.reddit.com/r/ocaml/comments/38zq6v/how_to_define_infix_functions_in_ocaml/
*)

open Arg
open Printf
open Angstrom

let is_even n = 
  n mod 2 = 0

(* https://en.wikipedia.org/wiki/Exponentiation_by_squaring *)
let int_pow base exponent =
  if exponent < 0 then invalid_arg "exponent can not be negative" else
  let rec aux accumulator base = function
    | 0 -> accumulator
    | 1 -> base * accumulator
    | e when is_even e -> aux accumulator (base * base) (e / 2)
    | e -> aux (base * accumulator) (base * base) ((e - 1) / 2) in
  aux 1 base exponent

let ( @< ) x f = int_pow f x;;

let parens p = char '(' *> p <* char ')'
let add = char '+' *> return (+)
let sub = char '-' *> return (-)
let mul = char '*' *> return ( * )
let div = char '/' *> return (/)
let pow = char '>' *> return (@<)

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let chainl1 e op =
  let rec go acc =
    (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
  e >>= fun init -> go init

(* todo: integer exponentiation *)
let expr : int t =
  fix (fun expr ->
    let factor = parens expr <|> integer in
    let term   = chainl1 factor (mul <|> div) in
    chainl1 term (add <|> sub))

let eval (str:string) : int =
  match parse_string expr str with
  | Ok v      -> v
  | Error msg -> failwith msg

(* command line logic follows *)

let input_text = ref ""
let result = ref 0

let () =
  begin
    Arg.parse [] (fun x -> input_text := x) "";
    try
      if !input_text = "" then
        raise (Arg.Bad ("missing argument: no expression given"))
      else
        printf " string = %s\n" !input_text;
        result := eval !input_text;
        printf " result = %s\n" (string_of_int !result)
    with
    | Arg.Bad msg ->
       eprintf "%s: %s\n" Sys.argv.(0) msg; eprintf "%s\n" ""; exit 1;
  end