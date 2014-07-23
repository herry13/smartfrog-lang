(*
  sflexer.mll - OCaml Lexer file (ocamllex)
  author: Herry (herry13@gmail.com)

  changelog:
  22.07.2014 - first released
*)

{
	open Lexing
	open Sfparser

	exception SyntaxError of string

	let next_line lexbuf =
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <-
			{
				pos with pos_bol = lexbuf.lex_curr_pos;
				pos_lnum = pos.pos_lnum + 1
			}
}

(* regular expressions *)
let int            = '-'? ['0'-'9']+
let digit          = ['0'-'9']
let frac           = '.' digit*
let exp            = ['e' 'E'] ['-' '+']? digit+
let float          = digit* frac? exp?
let string         = '"' ('\\'_|[^'\\' '"'])* '"'
let include_string = '"' ('\\'_|[^'\\' '"'])+ '"'
let white          = [' ' '\t']+
let newline        = '\r' | '\n' | "\r\n"
let ident          = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '.']*
let comment        = "//" [^'\n''\r']*
let comments       = '/' '*'+ (('*'[^'/'])+|[^'*']+)* '*'+ '/'
let include_file   = "#include"
let true_value     = "true"
let false_value    = "false"
let null_value     = "null"
let extends        = "extends"
let data_ref       = "DATA"

(* lexer rules *)
rule token =
	parse
	| white       { token lexbuf }
	| newline     { next_line lexbuf; token lexbuf }
	| comment     { token lexbuf }
	| '/' '*'+    { read_comments lexbuf; token lexbuf }
	| include_file white '"'
	              { INCLUDE (read_string (Buffer.create 17) lexbuf) }
	| ','         { COMMA }
	| '{'         { BEGIN }
	| '}'         { END }
	| '['         { LBRACKET }
	| ']'         { RBRACKET }
	| ';'         { EOS }
	| ':'         { SEP }
	| int         { INT (int_of_string (Lexing.lexeme lexbuf)) }
	| float       { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
	| true_value  { BOOL true }
	| false_value { BOOL false }
	| null_value  { NULL }
	| extends     { EXTENDS }
	| data_ref    { DATA }
	| '"'         { STRING (read_string (Buffer.create 17) lexbuf) }
	| ident       { ID (Lexing.lexeme lexbuf) }
	| eof         { EOF }

and read_string buf =
	parse
	| '"'           { Buffer.contents buf }
	| '\\' '/'      { Buffer.add_char buf '/'; read_string buf lexbuf }
	| '\\' '\\'     { Buffer.add_char buf '\\'; read_string buf lexbuf }
	| '\\' 'b'      { Buffer.add_char buf '\b'; read_string buf lexbuf }
	| '\\' 'f'      { Buffer.add_char buf '\012'; read_string buf lexbuf }
	| '\\' 'n'      { Buffer.add_char buf '\n'; read_string buf lexbuf }
	| '\\' 'r'      { Buffer.add_char buf '\r'; read_string buf lexbuf }
	| '\\' 't'      { Buffer.add_char buf '\t'; read_string buf lexbuf }
	| '\n'          { Buffer.add_char buf '\n'; next_line lexbuf; read_string buf lexbuf }
	| '\r'          { Buffer.add_char buf '\r'; read_string buf lexbuf }
	| [^ '"' '\\' '\n' '\r']+
	                {
	                	Buffer.add_string buf (Lexing.lexeme lexbuf);
	                	read_string buf lexbuf
	                }
	| _             { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
	| eof           { raise (SyntaxError ("String is not terminated")) }

and read_comments =
	parse
	| '*' '/'  { }
	| '\n'     { next_line lexbuf; read_comments lexbuf }
	| _        { read_comments lexbuf }
