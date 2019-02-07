(* 
ocamlbuild run_command.ml run_command.native -pkg unix && ./run_command.native

https://stackoverflow.com/questions/4350361/call-an-external-program-from-within-ocaml
http://www.martani.net/2010/04/ocaml-and-unix-programming.html

Seemingly best link, defines syscall:
https://www.rosettacode.org/wiki/Execute_a_system_command#OCaml
*)

open Unix
open Printf

let check_exit_status = function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED r -> Printf.eprintf "warning: the process terminated with exit code (%d)\n%!" r
  | Unix.WSIGNALED n -> Printf.eprintf "warning: the process was killed by a signal (number: %d)\n%!" n
  | Unix.WSTOPPED n -> Printf.eprintf "warning: the process was stopped by a signal (number: %d)\n%!" n

let syscall ?(env=[| |]) cmd =
  let ic, oc, ec = Unix.open_process_full cmd env in
  let buf1 = Buffer.create 96
  and buf2 = Buffer.create 48 in
  (try while true do Buffer.add_channel buf1 ic 1 done with End_of_file -> ());
  (try while true do Buffer.add_channel buf2 ec 1 done with End_of_file -> ());
  let exit_status = Unix.close_process_full (ic, oc, ec) in
  check_exit_status exit_status;
  (Buffer.contents buf1,
   Buffer.contents buf2)

let () = 
  let something1 = Unix.execv "cat addresses.csv" in
  let something2 = syscall "cat addresses.csv" in
  match something2 with 
    a, _ -> print_endline a