open Array

let help = "usage: csf <sf-file>\n\n"

let buf file =
	object
		val ic = open_in file
		method lex : (string -> int -> int) =
			fun s n -> input ic s 0 n
	end

let compile file =
	let ic = open_in file in
	try
		let b = buf file in
		let result = Sfparser.sf Sflexer.token (Lexing.from_function b#lex) in
			print_string (Domain.yaml_of_store result);
            print_newline();
            flush stdout

	with e ->
		close_in_noerr ic;
		raise e

let _ =
	if (length Sys.argv) < 2 then print_string help
	else compile Sys.argv.(1)
