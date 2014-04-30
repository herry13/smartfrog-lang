{
	open Sfparser
}
rule token = parse
	  [' ' '\t' '\n']                       { token lexbuf } (* skip blanks *)
	| '-'?['0'-'9']+ as lxm                 { INT(int_of_string lxm) }
    | '-'?['0'-'9']+ '.' ['0'-'9']* as lxm  { FLOAT(float_of_string lxm) }
	| "true"                                { BOOL(true) }
	| "false"                               { BOOL(false) }
    | "null"                                { NULL }
    | "extends"                             { EXTENDS }
    | ','                                   { COMMA }
    | '{'                                   { BEGIN }
    | '}'                                   { END }
    | '"'('\\'_|[^'\\''"'])*'"' as lxm      { STRING(lxm) }
	| ';'                                   { EOS } (* end of assignment *)
    | ':'                                   { SEP } (* identifiers' separator *)
	| ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as lxm	{ ID(lxm) }
	| eof                                   { EOF }
