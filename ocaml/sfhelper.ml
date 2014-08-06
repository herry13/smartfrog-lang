(**
 * sfhelper.ml - helper functions
 * author: Herry (herry13@gmail.com)
 *
 * changelog:
 * 22.07.2014 - first released
 *)

open Sfsyntax

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

(* (filename, line-number, column-number, token) *)
exception ParseError of string * int * int * string

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
				Sfparser.inblock_included (get_token lexstack) dummy_lexbuf
			with e -> check_error_sf_include e lexstack
		)
	| Sfparser.SFP_INCLUDE_FILE file ->
		Sfparser.SFP_INCLUDE
		(
			let lexstack = create file Sflexer.token in
			try
				Sfparser.incontext_included (get_token lexstack) dummy_lexbuf
			with e -> check_error_sfp e lexstack
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
 * then throw a ParseError exception containing a filename, a line number
 * a column number, and a last token that produces the error,
 * otherwise just throw again the exception
 *)
and check_error_sf_include e lexstack =
	match e with
	| Parsing.Parse_error ->
		let fname, lnum, lpos = current_pos lexstack in
		raise (ParseError (fname, lnum, lpos, (lexeme lexstack)))
	| e -> raise e

and check_error_sfp e lexstack =
	match e with
	| Parsing.Parse_error ->
		let fname, lnum, lpos = current_pos lexstack in
		raise (ParseError (fname, lnum, lpos, (lexeme lexstack)))
	| e -> raise e

and check_error e lexstack =
	match e with
	| Parsing.Parse_error ->
		let fname, lnum, lpos = current_pos lexstack in
		raise (ParseError (fname, lnum, lpos, (lexeme lexstack)))
	| e -> raise e
