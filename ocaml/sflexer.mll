{
	open Sfparser
}
rule token = parse
	  [' ''\t''\n''\r']                           { token lexbuf } (* skip blanks *)
    | "//"[^'\n''\r']*                            { token lexbuf }
    | '/' '*'+ (('*'[^'/'])+|[^'*']+)* '*'+ '/'   { token lexbuf }
	| '-'?['0'-'9']+ as i                         { INT(int_of_string i) }
    | '-'?['0'-'9']+ '.' ['0'-'9']* as f          { FLOAT(float_of_string f) }
	| "true"                                      { BOOL true }
	| "false"                                     { BOOL false }
    | "null"                                      { NULL }
    | "extends"                                   { EXTENDS }
    | "DATA"                                      { DATA }
	| "#include"								  { INCLUDE }
    | ','                                         { COMMA }
    | '{'                                         { BEGIN }
    | '}'                                         { END }
    | '['                                         { LBRACKET }
    | ']'                                         { RBRACKET }
    | '"'('\\'_|[^'\\''"'])*'"' as s              { STRING(String.sub s 1 ((String.length s)-2)) }
	| ';'                                         { EOS } (* end of assignment *)
    | ':'                                         { SEP } (* identifiers' separator *)
	| ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as id    { ID id }
	| eof                                         { EOF }
