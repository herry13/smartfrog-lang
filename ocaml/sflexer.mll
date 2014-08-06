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

	(* keywords *)
	let keywords = ["true"; "false"; "null"; "NULL"; "extends"; "DATA";
	                "isa"; "schema"; "bool"; "boolean"; "num"; "number";
	                "str"; "string"; "obj"; "object"; "include"; "import"]

	let is_keyword id =
		let rec check id words =
			match words with
			| head :: tail -> if head = id then true else check id tail
			| _ -> false
		in
		check id keywords
}

(* regular expressions *)
let int              = '-'? ['0'-'9']+
let digit            = ['0'-'9']
let frac             = '.' digit*
let exp              = ['e' 'E'] ['-' '+']? digit+
let float            = digit* frac? exp?
let string           = '"' ('\\'_|[^'\\' '"'])* '"'
let include_string   = '"' ('\\'_|[^'\\' '"'])+ '"'
let white            = [' ' '\t']+
let newline          = '\r' | '\n' | "\r\n"
let ident            = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '.']*
let comment          = "//" [^'\n''\r']*
let comments         = '/' '*'+ (('*'[^'/'])+|[^'*']+)* '*'+ '/'
let include_file     = "#include"
let sfp_include_file = "include" | "import"
let true_value       = "true"
let false_value      = "false"
let null_value       = "null" | "NULL"
let extends          = "extends"
let data_ref         = "DATA"
let isa              = "isa"
let schema           = "schema"
let t_bool           = "bool" | "boolean"
let t_num            = "num" | "number"
let t_str            = "str" | "string"
let t_obj            = "obj" | "object"

(* lexer rules *)
rule token =
	parse
	| white       { token lexbuf }
	| newline     { next_line lexbuf; token lexbuf }
	| comment     { token lexbuf }
	| '/' '*'+    { read_comments lexbuf; token lexbuf }
	| include_file white '"'
	              { INCLUDE (read_string (Buffer.create 17) lexbuf) }
	| sfp_include_file white '"'
	              { SFP_INCLUDE_FILE (read_string (Buffer.create 17) lexbuf) }
	| ','         { COMMA }
	| '{'         { BEGIN }
	| '}'         { END }
	| '['         { LBRACKET }
	| ']'         { RBRACKET }
	| ';'         { EOS }
	| '.'         { SEP }
	| ':'         { COLON }
	| '*'         { ASTERIX }
	| '='         { EQUAL }
	| int         { INT (Lexing.lexeme lexbuf) }
	| float       { FLOAT (Lexing.lexeme lexbuf) }
	| true_value  { BOOL "true" }
	| false_value { BOOL "false" }
	| null_value  { NULL }
	| extends     { EXTENDS }
	| data_ref    { DATA }
	| isa         { ISA }
	| schema      { SCHEMA }
	| t_bool      { TBOOL }
	| t_num       { TNUM }
	| t_str       { TSTR }
	| t_obj       { TOBJ }
	| '"'         { STRING (read_string (Buffer.create 17) lexbuf) }
	| ident       {
	                let id = Lexing.lexeme lexbuf in
	                if is_keyword id then raise (SyntaxError (id ^ " is a keyword"))
	                else ID id
	              }
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
