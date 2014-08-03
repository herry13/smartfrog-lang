(**
 * sfhelper.ml - helper functions
 * author: Herry (herry13@gmail.com)
 *
 * changelog:
 * 22.07.2014 - first released
 *)

open Sfast

(*******************************************************************
 * lexer helper type and functions
 *******************************************************************)

(**
 * The Lexstack type.
 *)
type 'a t =
	{
		mutable stack    : (string * in_channel * Lexing.lexbuf) list;
		mutable filename : string;
		mutable chan     : in_channel;
		mutable lexbuf   : Lexing.lexbuf;
		lexfunc          : Lexing.lexbuf -> 'a;
	}

(**
 * Create a lexstack with an initial top level filename and the lexer function
 *)
let create top_filename lexer_function =
	let chan = open_in top_filename in
	{
		stack = [];
		filename = top_filename;
		chan = chan;
		lexbuf = Lexing.from_channel chan;
		lexfunc = lexer_function
	}

(**
 * Get the current lexeme.
 *)
let lexeme ls = Lexing.lexeme ls.lexbuf

(**
 * Get filename, line number and column number of current lexeme.
 *)
let current_pos ls =
	let pos = Lexing.lexeme_end_p ls.lexbuf in
	let linepos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - String.length (Lexing.lexeme ls.lexbuf) in
	ls.filename, pos.Lexing.pos_lnum, linepos

(**
 * The next token need to accept an unused dummy lexbuf so that
 * a closure consisting of the function and a lexstack can be passed
 * to the ocamlyacc generated parser.
 *)
let rec get_token ls dummy_lexbuf =
	let token = ls.lexfunc ls.lexbuf in
	match token with
	| Sfparser.INCLUDE file -> (* parse included file *)
		(* this SF-style include (the included file must be legal statements) **)
		Sfparser.SF_INCLUDE
		(
			let lexstack = create file Sflexer.token in
			try 
				Sfparser.included (get_token lexstack) dummy_lexbuf
			with e -> check_error e lexstack
		)
	| Sfparser.EOF ->
		(
			match ls.stack with
			| [] -> Sfparser.EOF      (* buffer is empty, then return EOF *)
			| (fn, ch, lb) :: tail -> (* buffer isn't empty, then continue to parse top file *) 
				ls.filename <- fn;
				ls.chan <- ch;
				ls.stack <- tail;
				ls.lexbuf <- lb;
				get_token ls dummy_lexbuf
		)
  | _ -> token

(**
 * Catch the exception, when it is a parse error (Parse_error)
 * then throw a failure containing a string of error message,
 * otherwise just throw again the exception
 *
 * TODO - sometime error line and column number does not match
 *)
and check_error e lexstack =
	match e with
	| Parsing.Parse_error ->
		let fname, lnum, lpos = current_pos lexstack in
		let errstr = Printf.sprintf
			"\n\nFile '%s' line %d, column %d\ncurrent token is '%s'.\n\n"
			fname lnum lpos (lexeme lexstack) in
		raise (Failure errstr)
	| e -> raise e
	
