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

  let get_include_file s = create_string (String.trim (String.sub s 8 ((String.length s) - 9)))

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

(* lexer rules *)
rule token =
  parse
  | white     { token lexbuf }
  | newline   { next_line lexbuf; token lexbuf }
  | comment   { token lexbuf }
  | '/' '*'+  { read_comments lexbuf; token lexbuf }
  | "#include" white include_string white? ';' { INCLUDE (get_include_file (Lexing.lexeme lexbuf)) }
  | ','       { COMMA }
  | '{'       { BEGIN }
  | '}'       { END }
  | '['       { LBRACKET }
  | ']'       { RBRACKET }
  | ';'       { EOS }
  | ':'       { SEP }
  | int       { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float     { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"    { BOOL true }
  | "false"   { BOOL false }
  | "null"    { NULL }
  | "extends" { EXTENDS }
  | "DATA"    { DATA }
  | '"'       { read_string (Buffer.create 17) lexbuf }
  | ident     { ID (Lexing.lexeme lexbuf) }
  | eof       { EOF }

and read_string buf =
  parse
  | '"'           { STRING (Buffer.contents buf) }
  | '\\' '/'      { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'     { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'      { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'      { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'      { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'      { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'      { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\n'          { Buffer.add_char buf '\n'; next_line lexbuf; read_string buf lexbuf }
  | '\r'          { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | [^ '"' '\\' '\n' '\r']+ { Buffer.add_string buf (Lexing.lexeme lexbuf);
                              read_string buf lexbuf
                            }
  | _             { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { raise (SyntaxError ("String is not terminated")) }

and read_comments =
  parse
  | '*' '/'  { }
  | '\n'     { next_line lexbuf; read_comments lexbuf }
  | _        { read_comments lexbuf }
