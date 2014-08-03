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

(**
 * Parse main SF file.
 *
 * @param file - file to be parsed
 * @return Sfast._SF - an SF abstract syntax tree
 *)
let rec parse_file file =
	let dummy_lexbuf = Lexing.from_string "" in
	let lexstack = Sfhelper.create file Sflexer.token in
	let ast =
		try
			Sfparser.sf (Sfhelper.get_token lexstack) dummy_lexbuf
		with
			e -> Sfhelper.check_error e lexstack Sfast.EB
	in
	let store = eval_value ast in
	print_string (Sfdomainhelper.yaml_of_store store)

and eval_value ast = Sfvaluation.sfSpecification ast

(**
 * main function
 *)
let _ =
	if (length Sys.argv) < 2 then print_string help
	else if Sys.argv.(1) = "-h" || Sys.argv.(1) = "--help" then print_string help
	(* else print_string (Sfast.string_of_sf (parse_file Sys.argv.(1))) *)
	else parse_file Sys.argv.(1)

