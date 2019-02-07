(* 
Usage:
    ocamlbuild cli_is_prime.ml cli_is_prime.native && ./cli_is_prime.native 21
    
Check:
    https://www.tldp.org/LDP/abs/html/exitcodes.html
    https://www.cyberciti.biz/faq/shell-how-to-determine-the-exit-status-of-linux-and-unix-command/
    
    After running: 
        $ echo $?
    0 -> ok
    1 -> error
*)

open Printf

(* https://ocaml.org/learn/tutorials/99problems.html *)
let is_prime n =
  let n = abs n in
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
  n <> 1 && is_not_divisor 2;;

let () =
  (* 
  for i = 0 to Array.length Sys.argv - 1 do
    printf "[%i] %s\n" i Sys.argv.(i)
  *)
  (* let check = is_prime(int_of_string(Sys.argv.(1))) in *)
  try
    let check = Sys.argv.(1) |> int_of_string |> is_prime in
    printf "%s\n" (string_of_bool check);
    exit 0;
  with 
    (* https://www.systutorials.com/241733/how-to-print-a-line-to-stderr-and-stdout-in-ocaml/ *)
    _ -> 
        Printf.eprintf "Not an integer!\n"; (* prints to stderr instead of stdout *)
        exit 1;
