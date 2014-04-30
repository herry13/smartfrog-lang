%token <int> INT
%token <bool> BOOL
%token <string> ID
%token EOS EOF
%start sf           /* entry point */
%type <string> sf
%%
sf:
	body EOF		{ $1 }
;
body:
	  assignment		{ $1 }
	| body assignment	{ String.concat "; " [$1;$2] }
;
assignment:
	ID value	{ String.concat "=" [$1;$2] }
;
value:
	basic EOS	{ $1 }
;
basic:
	  BOOL	{ string_of_bool($1) }
	| INT	{ string_of_int($1) }
;
