(*
An example to parse command line arguments with the standard library (Arg module)

Compile with:
  ocamlbuild cli_arg.ml cli_arg.native

Original source:
  http://www.labri.fr/perso/fleury/posts/programming/back-to-ocaml.html

Other links:  
  https://stackoverflow.com/questions/18704894/using-ocaml-args-in-program
  https://scylardor.fr/2013/10/14/ocaml-parsing-a-programs-arguments-with-the-arg-module/
*)

open Arg
open Printf

(* Required positional arguments *)
let filename = ref ""

(* Default options *)
let verbosity = ref 0
let output = ref ""

(* version () : Display version and exit *)
let version () =
  (* printf "%s - %s\n" program_name program_version; *)
  print_endline "This software rebuild parts of the program control-flow";
  print_endline "based on an analysis of executable binary files.";
  exit 0;;

(** program usage *)
let usage = "usage: " ^ Sys.argv.(0) ^ " [-h|-V] [-v lvl] [-o file] filename"

(** program options *)
let speclist = [
  ("-output",  Arg.Set_string output, ": write output in file <file>");
  ("-verbose", Arg.Int (fun l -> verbosity := l), ": set verbosity level");
  ("-version", Arg.Unit version, ": display version and exit");
]

(** parse_args function *)
let parse_cmdline =
  begin
    Arg.parse speclist (fun x -> filename := x) usage;
    try
      if !filename = "" then
        raise (Arg.Bad ("missing argument: no input file name given"))
    with
    | Arg.Bad msg ->
       eprintf "%s: %s\n" Sys.argv.(0) msg; eprintf "%s\n" usage; exit 1;
  end

(* Main function *)
let () =
  begin
    parse_cmdline;
    printf "Running the rest of the program... with:\n";
    printf " verbosity = %d\n" !verbosity;
    printf " filename = %s\n" !filename;
  end