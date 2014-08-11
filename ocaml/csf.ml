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
           "\n  -fd     solve the problem using FastDownard search engine" ^
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

let fdr_temp_file = "output.sas"

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
				let fs = Sastranslator.FlatStore.make (eval_value (ast_of_file file1)) in
				Sastranslator.FlatStore.string_of fs
			| "-fdr"  -> Sastranslator.fdr (ast_of_file file1) (ast_of_file file2)
			| "-fd"   -> solve file1 file2
			| "-yaml" -> Sfdomainhelper.yaml_of_store (eval_value (ast_of_file file1))
			| _       -> Sfdomainhelper.json_of_store (eval_value (ast_of_file file1))
		with
		| Sftype.TypeError (code, msg) -> prerr_string (msg ^ "\n"); exit code
		| Sfdomain.SfError (code, msg) -> prerr_string (msg ^ "\n"); exit code
	in
	print_string (str ^ "\n")

and eval_value ast = Sfvaluation.sfpSpecification ast

and solve init goal =
	let fd_preprocessor = "FD_PREPROCESSOR" in
	let fd_search = "FD_SEARCH" in
	let sas_file = "output.sas" in
	let plan_file = "sas_plan" in
	let search_options = "--search \"lazy_greedy(ff())\"" in
	try
		let preprocessor = Sys.getenv fd_preprocessor in
		let search = Sys.getenv fd_search in
		if not (Sys.file_exists preprocessor) then (
			prerr_string ("Error: " ^ preprocessor ^ " is not exist!\n\n"); exit 1;
		);
		if not (Sys.file_exists search) then (
			prerr_string ("Error: " ^ search ^ " is not exist!\n\n"); exit 1;
		);
		let fdr = Sastranslator.fdr (ast_of_file init) (ast_of_file goal) in
		(* save FDR to sas_file *)
		let channel = open_out sas_file in
		output_string channel fdr;
		close_out channel;
		(* invoke preprocessor *)
		let cmd = preprocessor ^ " < " ^ sas_file in
		if not ((Sys.command cmd) = 0) then (prerr_string "Error: preprocessor failed\n\n"; exit 1);
		(* invoke search *)
		let cmd = search ^ " " ^ search_options ^ " < output" in
		if not ((Sys.command cmd) = 0) then (prerr_string "Error: search failed\n\n"; exit 1);
		(* read plan_file *)
		if Sys.file_exists plan_file then (
			let channel = open_in plan_file in
			let n = in_channel_length channel in
			let s = String.create n in
			really_input channel s 0 n;
			close_in channel;
			"\n\nSolution plan:\n" ^ s
		)
		else "No solution!"
	with
		e -> prerr_string "Error: cannot find FD_PREPROCESSOR or FD_SEARCH\n\n"; exit 1

(**
 * main function
 *)
let _ =
	let l = length Sys.argv in
	if l < 2 then print_string help
	else
		match Sys.argv.(1) with
		| "-fdr" when l = 4 -> parse_file "-fdr" Sys.argv.(2) Sys.argv.(3)
		| "-fd"  when l = 4 -> parse_file "-fd" Sys.argv.(2) Sys.argv.(3)
		| _      when l = 3 -> parse_file Sys.argv.(1) Sys.argv.(2) ""
		| _      when l = 2 -> parse_file "" Sys.argv.(1) ""
		| _                 -> print_string help
