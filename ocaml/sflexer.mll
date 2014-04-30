{
	open Sfparser
}
rule token = parse
	  [' ' '\t' '\n']			{ token lexbuf } (* skip blanks *)
	| ['0'-'9']+ as lxm			{ INT(int_of_string lxm) }
	| "true"					{ BOOL(true) }
	| "false"					{ BOOL(false) }
	| ';'						{ EOS } (* end of assignment *)
	| ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as lxm	{ ID(lxm) }
	| eof						{ EOF }
