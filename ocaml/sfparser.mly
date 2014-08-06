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
%token <string> SFP_INCLUDE_FILE
%token <Sfsyntax.block -> Sfsyntax.block> SF_INCLUDE
%token <Sfsyntax.sfpcontext -> Sfsyntax.sfpcontext> SFP_INCLUDE
%token EXTENDS COMMA DATA BEGIN END SEP NULL LBRACKET RBRACKET EOS EOF
%token ISA SCHEMA ASTERIX COLON TBOOL TNUM TSTR TOBJ
%token EQUAL

/* entry point for main-file is 'sfp', for included file is 'incontext_included' or 'inblock_included' */
%start inblock_included sfp incontext_included
%type <Sfsyntax.block -> Sfsyntax.block> inblock_included
%type <Sfsyntax.sfp> sfp
%type <Sfsyntax.sfpcontext -> Sfsyntax.sfpcontext> incontext_included

%%

sfp:
	| sfpcontext EOF { $1 EmptyContext }

incontext_included:
	| sfpcontext EOF { $1 }

sfpcontext:
	| SCHEMA schema sfpcontext   { fun c -> S_C ($2, $3 c) }
	| assignment sfpcontext      { fun c -> A_C ($1, $2 c) }
	| SFP_INCLUDE EOS sfpcontext { fun c -> $1 ($3 c) }
	|                            { fun c -> c }

inblock_included:
	| block EOF { $1 }

block:
	| assignment block     { fun b -> A_B ($1, $2 b) }
	| SF_INCLUDE EOS block { fun b -> $1 ($3 b) }
	|                      { fun b -> b }

assignment:
	| reference type_def value { ($1, $2, $3) }

value:
	| EQUAL basic EOS    { BV $2 }
	| link_reference EOS { LR $1 }
	| ISA ID protos      { P (SID $2, $3) }
	| protos             { P (EmptySchema, $1) }

protos:
	| EXTENDS prototypes { $2 }
	| BEGIN block END    { B_P ($2 EmptyBlock, EmptyPrototype) }

prototypes:
    | prototype COMMA prototypes { $1 $3 }
    | prototype                  { $1 EmptyPrototype }

prototype:
    | BEGIN block END { fun p -> B_P ($2 EmptyBlock, p) }
    | reference       { fun p -> R_P ($1, p) } 

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

schema:
	| ID super BEGIN block END { ($1, $2, $4 EmptyBlock) }

super:
	| EXTENDS ID { SID $2 }
	|            { EmptySchema }

type_def:
	| COLON ttype { $2 }
	|             { TUndefined }

ttype:
	| LBRACKET RBRACKET ttype { TVec $3 }
	| ASTERIX tau             { TRef $2 }
	| tau                     { TBasic $1 }

tau:
	| TBOOL { TBool }
	| TNUM  { TNum }
	| TSTR  { TStr }
	| TOBJ  { TObject }
	| ID    { TSchema ($1, TObject) }

%%
