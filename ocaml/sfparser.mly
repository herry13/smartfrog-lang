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
%token GLOBAL EQUAL NOT_EQUAL IF THEN IN NOT LPARENTHESIS RPARENTHESIS
%token COST CONDITIONS EFFECTS ACTION

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
	| GLOBAL global sfpcontext   { fun c -> G_C ($2, $3 c) }
	| SFP_INCLUDE EOS sfpcontext { fun c -> $1 ($3 c) }
	| assignment sfpcontext      { fun c -> A_C ($1, $2 c) }
	|                            { fun c -> c }

inblock_included:
	| block EOF { $1 }

block:
	| assignment block     { fun b -> A_B ($1, $2 b) }
	| GLOBAL global block  { fun b -> G_B ($2, $3 b) }
	| SF_INCLUDE EOS block { fun b -> $1 ($3 b) }
	|                      { fun b -> b }

assignment:
	| ACTION reference action  { ($2, TUndefined, $3) }
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
    | BOOL           { Boolean $1 }
    | INT            { Number $1 }
    | FLOAT          { Number $1 }
    | STRING         { String $1 }
    | data_reference { DR $1 }
    | NULL           { Null }
    | vector         { Vector $1 }

vector:
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

global:
	| sfp_constraint { $1 }

conjunction:
	| sfp_constraint conjunction { $1 :: $2 }
	|                            { [] }

disjunction:
	| sfp_constraint disjunction { $1 :: $2 }
	|                            { [] }

sfp_constraint:
	| equal                                 { Eq $1 }
	| BEGIN conjunction END                 { And $2 }
	| LPARENTHESIS disjunction RPARENTHESIS { Or $2 }
	| not_equal                             { Ne $1 }
	| negation                              { Not $1 }
	| implication                           { Imply $1 }
	| membership                            { In $1 }

equal:
	| reference EQUAL basic EOS { ($1, $3) }

not_equal:
	| reference NOT_EQUAL basic EOS { ($1, $3) }

implication:
	| IF sfp_constraint THEN sfp_constraint { ($2, $4) }

negation:
	| NOT sfp_constraint { $2 }

membership:
	| reference IN vector EOS { ($1, $3) }

action:
	| parameters BEGIN cost conditions EFFECTS BEGIN effects END END
		{
			Ac ($1, $3, $4, $7)
		}

parameters:
	| LPARENTHESIS params RPARENTHESIS { $2 }
	|                                  { [] }

params:
	| param COMMA params { $1 :: $3 }
	| param              { [$1] }

param:
	| ID COLON ttype { ($1, $3) }

cost:
	| COST EQUAL INT EOS { Cost $3 }
	|                    { EmptyCost }

conditions:
	| CONDITIONS sfp_constraint { Cond $2 }
	|                           { EmptyCondition }

effects:
	| effect effects { $1 :: $2 }
	| effect         { [$1] }

effect:
	| reference EQUAL basic EOS { ($1, $3) }

%%
