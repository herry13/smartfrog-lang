open Array

let help = "usage: csf <sf-file>\n\n"

let compile file =
	let ic = open_in file in
	try
		let lexbuf = Lexing.from_channel ic in
		let result = Sfparser.sf Sflexer.token lexbuf in
			print_string (Domain.yaml_of_store result);
            print_newline();
            flush stdout

	with e ->
		close_in_noerr ic;
		raise e

let _ =
	if (length Sys.argv) < 2 then print_string help
	else compile Sys.argv.(1)
