/***
 * sfparser.ml - OCaml Yacc file (ocamlyacc)
 * author: Herry (herry13@gmail.com)
 *
 * changelog:
 * 22.07.2014 - first released
 ***/

%{

open Sfdomain

%}

%token <bool>   BOOL
%token <int>    INT
%token <float>  FLOAT
%token <string> STRING
%token <string> ID
%token <string> INCLUDE
%token <string list -> Sfdomain.store -> Sfdomain.store> SF_INCLUDE
%token EXTENDS COMMA DATA BEGIN END SEP NULL LBRACKET RBRACKET EOS EOF

/* entry point for main-file is 'sf', for included-file is 'included' */
%start sf included
%type <Sfdomain.store> sf
%type <string list -> Sfdomain.store -> Sfdomain.store> included

%%
sf:
	| body EOF
		{
			let s1 = $1 [] [] in
			let r = ["sfConfig"] in
			let v1 = find s1 r in
			match v1 with
			| Val (Store vs1) ->
				(
					let s2 = accept s1 r vs1 r in
					match find s2 r with
					| Val (Store v2) -> v2
					| _ -> failure 10
				)
			| _ -> failure 10
		}

included:
	| body EOF { $1 }

body:
	| assignment body { fun ns s -> $2 ns ($1 ns s) }
	| SF_INCLUDE EOS  { $1 }
	|                 { fun ns s -> s }

assignment:
	| reference value { fun ns s -> $2 ns (ref_plus_ref ns $1) s }

value:
	| basic EOS          { fun ns r s -> bind s r (Basic $1) }
	| EXTENDS prototypes { fun ns r s -> $2 ns r (bind s r (Store [])) }
	| link_reference EOS { fun ns r s -> bind s r ($1 r) }

prototypes:
    | prototype COMMA prototypes { fun ns r s -> $3 ns r ($1 ns r s) }
    | prototype                  { $1 }

prototype:
    | BEGIN body END { fun ns r s -> $2 r s }
    | reference      { fun ns r s -> inherit_proto s ns $1 r }

basic:
    | BOOL                { Bool $1 }
    | INT                 { Num (Int $1) }
    | FLOAT               { Num (Float $1) }
    | DATA data_reference { Ref $2 }
    | STRING              { Str $1 }
    | NULL                { Null }
    | vectors             { Vec $1 }

vectors:
    | LBRACKET items RBRACKET { $2 }

items:
    | basic COMMA items { $1 :: $3 }
    | basic             { [$1] }

link_reference:
    | reference { fun r -> if $1 <= r then failure 4 else Basic (Link $1) }

data_reference:
    | reference { $1 }

reference:
    | ID SEP reference { $1 :: $3 }
    | ID               { [$1] }

%%
