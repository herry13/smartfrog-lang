(*
  csf.ml - main file
  author: Herry (herry13@gmail.com)

  usage: csfp [option] <sf-file>
  where [option] is:
  -json   print store in JSON (default)
  -yaml   print store in YAML
  -ast    print abstract syntax tree


  change log:
*)

open Array

(* help message *)
let help = "usage: csfp [option] <sfp-file> [sfp-goal-file]" ^
           "\n\nwhere [option] is:" ^
           "\n  -ast    print abstract syntax tree" ^
           "\n  -type   evaluate types and print the type environment" ^
		   "\n  -json   evaluate values and print store in JSON (default)" ^
		   "\n  -yaml   evaluate values and print store in YAML" ^
		   "\n  -fs     evaluate values and print flat store" ^
		   "\n  -fdr    generate Finite Domain Representation (FDR)" ^
           "\n          [sfp-goal-file] must be provided" ^
           "\n\n"

let ast_of_file file =
	let dummy_lexbuf = Lexing.from_string "" in
	let lexstack = Sfhelper.create file Sflexer.token in
	try
		Sfparser.sfp (Sfhelper.get_token lexstack) dummy_lexbuf
	with e ->
		try
			Sfhelper.check_error e lexstack
		with
		| Sfhelper.ParseError (fname, lnum, lpos, token) ->
			prerr_string ("--- Parse error ---\nfile:   " ^ fname ^ "\nline:   " ^ (string_of_int lnum) ^
			              "\ncolumn: " ^ (string_of_int lpos) ^ "\ntoken:  '" ^ token ^ "'\n\n");
			exit 1
		| e -> raise e

(**
 * Parse main SF file.
 *
 * @param file file to be parsed
 * @param opt  an option       
 * @return Sfsyntax.sf an SF abstract syntax tree
 *)
let rec parse_file opt file1 file2 =
	let str =
		try
			match opt with
			| "-ast"  -> Sfsyntax.string_of_sfp (ast_of_file file1)
			| "-type" ->
				let env = Sftype.sfpSpecification (ast_of_file file1) in
				Sftype.string_of_env env
			| "-fs"   ->
				let fs = Sastranslator.normalise (eval_value (ast_of_file file1)) in
				Sastranslator.string_of_flat_store fs
			| "-fdr"  -> Sastranslator.fdr (ast_of_file file1) (ast_of_file file2)
			| "-yaml" -> Sfdomainhelper.yaml_of_store (eval_value (ast_of_file file1))
			| _       -> Sfdomainhelper.json_of_store (eval_value (ast_of_file file1))
		with
		| Sftype.TypeError (code, msg) -> prerr_string (msg ^ "\n"); exit code
		| Sfdomain.SfError (code, msg) -> prerr_string (msg ^ "\n"); exit code
	in
	print_string (str ^ "\n")

and eval_value ast = Sfvaluation.sfpSpecification ast

(**
 * main function
 *)
let _ =
	let l = length Sys.argv in
	if l < 2 then print_string help
	else
		match Sys.argv.(1) with
		| "-fdr" when l = 4 -> parse_file "-fdr" Sys.argv.(2) Sys.argv.(3)
		| _      when l = 3 -> parse_file Sys.argv.(1) Sys.argv.(2) ""
		| _      when l = 2 -> parse_file "" Sys.argv.(1) ""
		| _                 -> print_string help
