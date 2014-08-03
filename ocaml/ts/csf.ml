(*
  csf.ml - main file
  author: Herry (herry13@gmail.com)

  usage: csf <sf-file>

  changelog:
*)

open Array

(* help message *)
let help = "usage: csf <sf-file>" ^
           "\n\n"

(*
	parse main SF file
	@file - file to be parsed
*)
let parse_file file =
	let dummy_lexbuf = Lexing.from_string "" in
	let lexstack = Sfhelper.create file Sflexer.token in
	try
		Sfparser.sf (Sfhelper.get_token lexstack) dummy_lexbuf
	with
		e -> Sfhelper.check_error e lexstack Sfast.EB


(* main function *)
let _ =
	if (length Sys.argv) < 2 then print_string help
	else if Sys.argv.(1) = "-h" || Sys.argv.(1) = "--help" then print_string help
	else print_string (Sfast.string_of_sf (parse_file Sys.argv.(1)))

