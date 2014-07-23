(*
  csf.ml - main file
  author: Herry (herry13@gmail.com)

  usage: csf [option] <sf-file>

  options:
  -sf     print output in plain SF
  -json   print output in JSON (default)
  -yaml   print output in YAML
  -xml    print output in XML

  changelog:
  22.07.2014 - first released
*)

open Array

(* help message *)
let help = "usage: csf [option] <sf-file>" ^
           "\n\nwhere [option] is:" ^
		   "\n  -sf     print output in plain SF" ^
		   "\n  -json   print output in JSON (default)" ^
		   "\n  -yaml   print output in YAML" ^
		   "\n  -xml    print output in XML\n\n"

(*
	parse main SF file
	@file - file to be parsed
*)
let parse_file file =
	let dummy_lexbuf = Lexing.from_string "" in
	let lexstack = Sfhelper.create file Sflexer.token in
	try
		Sfparser.sf (Sfhelper.get_token lexstack) dummy_lexbuf
	with e -> Sfhelper.check_error e lexstack

(*
	parse file with given option
	@opt - option
	@file - file to be parsed	
*)
let parse_option_file opt file =
	let result = parse_file file in
	if opt = "-yaml" then print_string (Sfhelper.yaml_of_store result)
	else if opt = "-xml" then print_string (Sfhelper.xml_of_store result)
	else if opt = "-sf" then print_string (Sfhelper.sf_of_store result)
	else print_string (Sfhelper.json_of_store result);
	print_newline()

(* main function *)
let _ =
	if (length Sys.argv) < 2 then print_string help
	else if Sys.argv.(1) = "-h" || Sys.argv.(1) = "--help" then print_string help
	else if (length Sys.argv) >= 3 then parse_option_file Sys.argv.(1) Sys.argv.(2)
	else parse_option_file "" Sys.argv.(1)
