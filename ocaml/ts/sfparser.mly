/**
 * sfparser.ml - OCaml Yacc file (ocamlyacc)
 * author: Herry (herry13@gmail.com)
 *
 * changelog:
 * 22.07.2014 - first released
 */

%{

open Sfast

%}

%token <string> BOOL
%token <string> INT
%token <string> FLOAT
%token <string> STRING
%token <string> ID
%token <string> INCLUDE
%token <Sfast._B -> Sfast._B> SF_INCLUDE
%token EXTENDS COMMA DATA BEGIN END SEP NULL LBRACKET RBRACKET EOS EOF

/* entry point for main-file is 'sf', for included-file is 'included' */
%start sf included
%type <Sfast._SF> sf
%type <Sfast._B -> Sfast._B> included

%%
sf:
	| body EOF { $1 EB }

included:
	| body EOF { $1 }

body:
	| assignment body      { fun b -> AB ($1, $2 b) }
	| SF_INCLUDE EOS body  { fun b -> $1 ($3 b) }
	|                      { fun b -> b }

assignment:
	| reference value { ($1, $2) }

value:
	| basic EOS          { BV $1 }
	| EXTENDS prototypes { P $2 }
	| link_reference EOS { LR $1 }

prototypes:
    | prototype COMMA prototypes { $1 $3 }
    | prototype                  { $1 EP }

prototype:
    | BEGIN body END { fun p -> BP ($2 EB, p) }
    | reference      { fun p -> RP ($1, p) } 

basic:
    | BOOL                { Bool $1 }
    | INT                 { Num $1 }
    | FLOAT               { Num $1 }
    | STRING              { Str $1 }
    | DATA data_reference { DR $2 }
    | NULL                { Null }
    | vectors             { Vec $1 }

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
