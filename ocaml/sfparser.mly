/**
 * sfparser.ml - OCaml Yacc file (ocamlyacc)
 * author: Herry (herry13@gmail.com)
 *
 * changelog:
 * 22.07.2014 - first released
 */

%{

open Sfsyntax

%}

%token <string> BOOL
%token <string> INT
%token <string> FLOAT
%token <string> STRING
%token <string> ID
%token <string> INCLUDE
%token <Sfsyntax.block -> Sfsyntax.block> SF_INCLUDE
%token EXTENDS COMMA DATA BEGIN END SEP NULL LBRACKET RBRACKET EOS EOF

/* entry point for main-file is 'sf', for included-file is 'included' */
%start sf included
%type <Sfsyntax.sf> sf
%type <Sfsyntax.block -> Sfsyntax.block> included

%%
sf:
	| body EOF { $1 EmptyBlock }

included:
	| body EOF { $1 }

body:
	| assignment body      { fun b -> A_B ($1, $2 b) }
	| SF_INCLUDE EOS body  { fun b -> $1 ($3 b) }
	|                      { fun b -> b }

assignment:
	| reference value { ($1, Sfsyntax.TUndefined, $2) }

value:
	| basic EOS          { BV $1 }
	| EXTENDS prototypes { P $2 }
	| link_reference EOS { LR $1 }

prototypes:
    | prototype COMMA prototypes { $1 $3 }
    | prototype                  { $1 EmptyPrototype }

prototype:
    | BEGIN body END { fun p -> B_P ($2 EmptyBlock, p) }
    | reference      { fun p -> R_P ($1, p) } 

basic:
    | BOOL                { Boolean $1 }
    | INT                 { Number $1 }
    | FLOAT               { Number $1 }
    | STRING              { String $1 }
    | DATA data_reference { DR $2 }
    | NULL                { Null }
    | vectors             { Vector $1 }

vectors:
    | LBRACKET items RBRACKET { $2 }

items:
    | basic COMMA items { $1 :: $3 }
    | basic             { [$1] }

link_reference:
    | reference { $1 }

data_reference:
    | reference { $1 }

reference:
    | ID SEP reference { $1 :: $3 }
    | ID               { [$1] }

%%
