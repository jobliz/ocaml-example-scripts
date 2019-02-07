(*
ocamlbuild csv_2.ml csv_2.native -pkg csv && ./csv_2.native

https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml
https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/23456034
https://ocaml.org/learn/tutorials/file_manipulation.html
https://ocaml.org/learn/tutorials/streams.html

let lines = In_channel.read_lines file in

(* List.iter ~f: handle_line lines; *) (* ~f here seems to be like a python kwarg, prints all lines separately *)

try
  while true do
    let line = input_line in_channel in
    (* do something with line *)
    printf line;
  done
  
https://www2.lib.uchicago.edu/keith/ocaml-class/io.html
OCaml's I/O system is completely imperative. You can use the high-level routines in the Pervasives module module, or the Posix-compliant routines in the Unix module (note that much of the Unix module will also work on Posix-compliant non-Unix systems like Windows). This page will cover only the Pervasives system, and will just cover the essential routines that are needed in most programs. See the documentation for more. 

https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml
https://stackoverflow.com/questions/10068713/string-to-list-of-char
https://ocaml.org/learn/tutorials/file_manipulation.html
*)

(* open Core *) (* https://www.reddit.com/r/ocaml/comments/69gzqd/ocaml_on_vs_code_merlin_doesnt_recognize_core/ *)
open Printf
open List

(* https://ocaml.org/learn/tutorials/streams.html *)
let line_stream_of_channel channel =
    Stream.from
      (fun _ ->
         try Some (input_line channel) with End_of_file -> None);;

let () = 
  print_endline "Imperative style, no Core used \n"; (* an imperative statement? *)
  let lines = ref [] in
  let in_chan = open_in "addresses.csv" in 
  
  try
    while true do
      let line = input_line in_chan in
      print_endline line;
      lines := line :: !lines; (* what does this do??? *)
      (* printf (string_of_int 52); *) (* (List.length(lines)); *)
    done
  with End_of_file ->
    close_in in_chan;
    print_endline "lines"; (* closes the try/with *)
    
  ;; (* closes the outer let() scope *)

let () =
  printf "\nUsing Stream.iter\n\n";
  let in_channel = open_in "addresses.csv" in 
  (* let line_stream = line_stream_of_channel in_chan in *)
  
  try
    Stream.iter
      (fun line ->
        (* do something with line *)
        print_endline line)
      (line_stream_of_channel in_channel);
    close_in in_channel;
  with e ->
    close_in in_channel;
  ;;
  