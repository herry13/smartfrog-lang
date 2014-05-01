{
	open Sfparser
}
rule token = parse
	  [' ''\t''\n''\r']                           { token lexbuf } (* skip blanks *)
    | "//"[^'\n''\r']*                            { token lexbuf }
    | '/' '*'+ (('*'[^'/'])+|[^'*']+)* '*'+ '/'   { token lexbuf }
	| '-'?['0'-'9']+ as lxm                       { INT(int_of_string lxm) }
    | '-'?['0'-'9']+ '.' ['0'-'9']* as lxm        { FLOAT(float_of_string lxm) }
	| "true"                                      { BOOL(true) }
	| "false"                                     { BOOL(false) }
    | "null"                                      { NULL }
    | "extends"                                   { EXTENDS }
    | "DATA"                                      { DATA }
	| "#include"								  { INCLUDE }
    | ','                                         { COMMA }
    | "++"                                        { MERGE }
    | '{'                                         { BEGIN }
    | '}'                                         { END }
    | '['                                         { LBRACKET }
    | ']'                                         { RBRACKET }
    | '"'('\\'_|[^'\\''"'])*'"' as lxm            { STRING(lxm) }
	| ';'+                                        { EOS } (* end of assignment *)
    | ':'                                         { SEP } (* identifiers' separator *)
	| ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as lxm	{ ID(lxm) }
	| eof                                         { EOF }
