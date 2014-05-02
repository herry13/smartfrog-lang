open Array

let help = "usage: csf [option] <sf-file>\n\nwhere [option] is:\n  -json   print output in JSON\n  -yaml   print output in YAML\n  -xml    print output in XML\n\n"

let compile opt file =
	let ic = open_in file in
	try
		let lexbuf = Lexing.from_channel ic in
		let result = Sfparser.sf Sflexer.token lexbuf in
			if opt = "-yaml" then
				print_string ((Domain.yaml_of_store result) ^ "\n")
			else if opt = "-xml" then
				print_string ((Domain.xml_of_store result) ^ "\n")
			else
				print_string ((Domain.json_of_store result) ^ "\n")

	with e ->
		close_in_noerr ic;
		raise e

let _ =
	if (length Sys.argv) < 2 then print_string help
	else if Sys.argv.(1) = "-h" || Sys.argv.(1) = "--help" then print_string help
	else if (length Sys.argv) >= 3 then compile Sys.argv.(1) Sys.argv.(2)
	else compile "" Sys.argv.(1)
