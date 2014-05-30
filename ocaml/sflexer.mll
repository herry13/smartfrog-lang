{
  open Lexing
  open Sfparser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }

  let create_string s = String.sub s 1 ((String.length s) - 2)

  let get_include_file1 s =
    if (String.get s ((String.length s) - 1)) == ';' then
      create_string (String.trim (String.sub s 8 ((String.length s) - 9)))
    else
      create_string (String.trim (String.sub s 8 ((String.length s) - 8)))

  let get_include_file s =
    let s1 = get_include_file1 s in
    if (String.get s1 0) == '/' then "." ^ s1
    else s1

}

(* regular expressions *)
let int     = '-'? ['0'-'9']+
let digit   = ['0'-'9']
let frac    = '.' digit*
let exp     = ['e' 'E'] ['-' '+']? digit+
let float   = digit* frac? exp?
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let ident   = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '.']*
let comment = "//" [^'\n''\r']*

(* main lexer rules *)
rule token =
  parse
  | white       { token lexbuf }
  | newline     { new_line lexbuf; token lexbuf }
  | comment     { token lexbuf }
  | '/' '*'+    { read_comments lexbuf; token lexbuf }
  | ','         { COMMA }
  | '{'         { BEGIN }
  | '}'         { END }
  | '[' | "[|"  { LBRACKET }
  | ']' | "|]"  { RBRACKET }
  | ';'         { EOS }
  | ':'         { SEP }
  | "--"        { ANY_ID }
  | int         { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float       { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"      { BOOL true }
  | "false"     { BOOL false }
  | "NULL"      { NULL }
  | '"'         { STRING (read_string (Buffer.create 17) lexbuf) }
  | "##"        { STRING (read_multiline_string (Buffer.create 17) lexbuf) }
  | "extends"   { EXTENDS }
  | "TBD"       { TOK_TBD }
  | "DATA"      { DATA }
  | "LAZY"      { LAZY }
  | "#include"[' ''\t']+ '"'('\\'_|[^'\\''"'])+'"' [' ''\t']* ';' as s
      { INCLUDE (get_include_file s) } (* return included file name *)
  | "#include"[' ''\t']+ '"'('\\'_|[^'\\''"'])+'"' [' ''\t']* as s
      { INCLUDE (get_include_file s) } (* return included file name *)
  | '('([^')'])*')'  { FUNCTION }
  | ident            { ID (Lexing.lexeme lexbuf) }
  | eof              { EOF }

(* lexer rules for parsing an inline string *)
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
  | [^ '"' '\\' '\n' '\r']+ { Buffer.add_string buf (Lexing.lexeme lexbuf);
                              read_string buf lexbuf
                            }
  | '\n'          { raise (SyntaxError ("Illegal new line character '\\n'")) }
  | '\r'          { raise (SyntaxError ("Illegal new line character '\\r'")) }
  | _             { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { raise (SyntaxError ("String is not terminated")) }

(* lexer rules for parsing a multiline string *)
and read_multiline_string buf =
  parse
  | '#'           { Buffer.contents buf }
  | '\n'          { Buffer.add_char buf '\n'; next_line lexbuf; read_multiline_string buf lexbuf }
  | '\r'          { Buffer.add_char buf '\r'; read_multiline_string buf lexbuf }
  | [^ '#' '\n' '\r']+ { Buffer.add_string buf (Lexing.lexeme lexbuf);
                         read_multiline_string buf lexbuf
                       }
  | _             { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { raise (SyntaxError ("String is not terminated")) }

(* lexer rules for parsing a multiline comments *)
and read_comments =
  parse
  | '*' '/'  { }
  | '\n'     { next_line lexbuf; read_comments lexbuf }
  | _        { read_comments lexbuf }

