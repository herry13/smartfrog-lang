(*
  csf.ml - main file
  author: Herry (herry13@gmail.com)
*)

open Array

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


let fd_plan init goal =
	let fd_preprocessor = "FD_PREPROCESS" in
	let fd_search       = "FD_SEARCH" in
	let fd_option       = "FD_OPTIONS" in
	let sas_file        = "output.sas" in
	let plan_file       = "sas_plan" in
	let default_search_options = "--search \"lazy_greedy(ff())\"" in
	try
		let preprocessor   = Sys.getenv fd_preprocessor in
		let search         = Sys.getenv fd_search in
		let search_options = try Sys.getenv fd_option with e -> default_search_options
		in
		if not (Sys.file_exists preprocessor) then (
			prerr_string ("Error: " ^ preprocessor ^ " is not exist!\n\n"); exit 1;
		);
		if not (Sys.file_exists search) then (
			prerr_string ("Error: " ^ search ^ " is not exist!\n\n"); exit 1;
		);
		let fdr = Fdr.of_sfp (ast_of_file init) (ast_of_file goal) in
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
		let plan =
			(* read plan_file *)
			if Sys.file_exists plan_file then (
				let channel = open_in plan_file in
				let n       = in_channel_length channel in
				let s       = String.create n in
				really_input channel s 0 n;
				close_in channel;
				"\n\nSolution plan:\n" ^ s
			)
			else "No solution!"
		in
		try
			let _ = Sys.getenv "FD_DEBUG" in
			plan
		with
			e ->
				try
					Sys.remove sas_file;
					Sys.remove "plan_numbers_and_cost";
					Sys.remove "output";
					Sys.remove plan_file;
					plan
				with e -> plan
	with
		e -> prerr_string "Error: environment variable FD_PREPROCESS or FD_SEARCH is not defined.\n\n"; exit 1


let usage_msg = "usage: csfp [options]\n\nwhere [options] are:";;
let opt_init_file = ref "";;
let opt_goal_file = ref "";;

(**
 * main function
 *)
let main =
	let do_compile = fun mode file ->
		let store = Sfvaluation.sfpSpecification (ast_of_file file) in
		match mode with
		| 1 -> print_endline (Sfdomain.json_of_store store)
		| 2 -> print_endline (Sfdomain.yaml_of_store store)
		| 3 -> let fs = Sfdomain.normalise store in
		       print_endline (Sfdomain.string_of_flatstore fs)
		| _ -> print_endline usage_msg
	in
	let do_ast = fun file -> print_endline (Sfsyntax.string_of_sfp (ast_of_file file))
	in
	let do_type = fun file -> print_endline (Sftype.string_of_env (Sftype.sfpSpecification (ast_of_file file)))
	in
	let verify_files () =
		if !opt_init_file = "" then (print_endline "Error: -init <file.sfp> is not set"; exit 1);
		if !opt_goal_file = "" then (print_endline "Error: -goal <file.sfp> is not set"; exit 1);
	in
	let do_fdr = fun () ->
		verify_files();
		print_endline (Fdr.of_sfp (ast_of_file !opt_init_file) (ast_of_file !opt_goal_file))
	in
	let do_fd = fun mode ->
		verify_files();
		print_endline (fd_plan !opt_init_file !opt_goal_file)
	in
	let speclist = [
			("-json", Arg.String (do_compile 1),    " Compile and print the result in JSON (default).");
			("-yaml", Arg.String (do_compile 2),    " Compile and print the result in YAML.");
			("-ast",  Arg.String do_ast,            "  Print abstract syntax tree.");
			("-type", Arg.String do_type,           " Evaluate and print the element types.");
			("-fs",   Arg.String (do_compile 3),    "   Compile and print the flat store.");
			("-init", Arg.Set_string opt_init_file, " File specification of initial state.");
			("-goal", Arg.Set_string opt_goal_file, " File specification of goal state.");
			("-fdr",  Arg.Unit do_fdr,              "  Generate and print Finite Domain Representation (FDR);" ^
			 "\n         -init <file1.sfp> -goal <file2.sfp> must be provided.");
			("-fd",   Arg.Unit do_fd,               "   Solve the SFP task using FastDownward search engine;" ^
			 "\n         -init <file1.sfp> -goal <file2.sfp> must be provided;" ^
			 "\n         environment variable FD_PREPROCESS & FD_SEARCH must be set;" ^
			 "\n         define FD_OPTIONS to pass options to the search engine;" ^
			 "\n         define FD_DEBUG to keep all output files.")
		]
	in
	Arg.parse speclist print_endline usage_msg;
	if (Array.length Sys.argv) < 2 then Arg.usage speclist usage_msg;;

let _ = main
