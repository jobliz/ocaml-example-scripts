(*
https://www2.lib.uchicago.edu/keith/ocaml-class/apps.html

ocamlbuild simple_cat.ml simple_cat.native

Renamed String.create to Bytes.create
*)

let cat filename =
  let chan = open_in filename in
  let size = 4 * 1024 in
  let buffer = Bytes.create size in
  let eof = ref false in
    while not !eof do
      let len = input chan buffer 0 size in
	if len > 0
	then print_string (String.sub buffer 0 len)
	else eof := true
    done

let main () =
  let len = (Array.length Sys.argv) in
  let argv = (Array.sub Sys.argv 1 (len-1)) in (* skip argv0 *)
    Array.iter cat argv

let _ = main ()
