(*
  csf.ml - main file
  author: Herry (herry13@gmail.com)

  usage: csf [option] <sf-file>
  where [option] is:
  -sf     print store in plain SF
  -json   print store in JSON (default)
  -yaml   print store in YAML
  -xml    print store in XML
  -ast    print abstract syntax tree


  change log:
*)

open Array

(* help message *)
let help = "usage: csf [option] <sf-file>" ^
           "\n\nwhere [option] is:" ^
		   "\n  -sf     print store in plain SF" ^
		   "\n  -json   print store in JSON (default)" ^
		   "\n  -yaml   print store in YAML" ^
		   "\n  -xml    print store in XML" ^
           "\n  -ast    print abstract syntax tree" ^
           "\n\n"

(**
 * Parse main SF file.
 *
 * @param file file to be parsed
 * @param opt  an option       
 * @return Sfast._SF an SF abstract syntax tree
 *)
let rec parse_file opt file =
	let dummy_lexbuf = Lexing.from_string "" in
	let lexstack = Sfhelper.create file Sflexer.token in
	let ast =
		try
			Sfparser.sf (Sfhelper.get_token lexstack) dummy_lexbuf
		with
			e -> Sfhelper.check_error e lexstack Sfast.EB
	in
	let str =
		if opt = "-ast" then Sfast.string_of_sf ast
		else
			let store = eval_value ast in
			if opt = "-yaml" then Sfdomainhelper.yaml_of_store store
			else if opt = "-sf" then Sfdomainhelper.sf_of_store store
			else if opt = "-xml" then Sfdomainhelper.xml_of_store store
			else Sfdomainhelper.json_of_store store
	in
	print_string (str ^ "\n")

and eval_value ast = Sfvaluation.sfSpecification ast

(**
 * main function
 *)
let _ =
	if (length Sys.argv) < 2 || (length Sys.argv) > 3 then print_string help
	else if Sys.argv.(1) = "-h" || Sys.argv.(1) = "--help" then print_string help
	else if (length Sys.argv) = 3 then parse_file Sys.argv.(1) Sys.argv.(2)
	else parse_file "" Sys.argv.(1)
