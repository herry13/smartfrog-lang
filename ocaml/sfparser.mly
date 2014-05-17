%{

open Domain

%}

%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID
%token <string> INCLUDE
%token <string list -> Domain.store -> Domain.store> SF_INCLUDE
%token TOK_TBD ANY_ID
%token EXTENDS COMMA DATA BEGIN END SEP NULL LBRACKET RBRACKET EOS EOF

%start sf included          /* entry points: sf -> main file, included -> included file */
%type <Domain.store> sf
%type <string list -> Domain.store -> Domain.store> included

%%
sf:
    | body EOF { 
                 let root = $1 [] [] in
                 let v = find root ref_main in
                 match v with
                 | Val (Store main) -> (
                     match find (accept main root ref_main replace_link) ref_main with
                     | Val (Store main1) -> main1
                     | _ -> raise (Failure "[err7]") )
                 | _ -> raise (Failure "[err7]")
               }

included:
    | body EOF   { $1 }

body:
    | assignment body     { fun ns s -> $2 ns ($1 ns s) }
    | SF_INCLUDE body     { fun ns s -> $2 ns ($1 ns s) }
    |                     { fun ns s -> s }

assignment:
    | tag reference value eos
      {
        fun ns s -> let r = $2 in
                    let (ns1, v1) = resolve s ns (prefix r) in
                    if (List.length r) = 1 then $3 ns (List.append ns r) s
                    else match v1 with
                         | Val (Store s1) -> $3 ns (List.append ns1 r) s
                         | _ -> raise (Failure ("[err6] prefix of " ^ (String.concat ":" r) ^ " is not a component"))
      }

value:
    | EOS                   { fun ns r s -> bind s r (Basic Null) }
    | TOK_TBD EOS           { fun ns r s -> bind s r TBD }
	| basic EOS             { fun ns r s -> bind s r (Basic $1) }
    | EXTENDS prototypes    { fun ns r s -> $2 ns r (bind s r (Store [])) }
    | link_reference EOS    { fun ns r s -> bind s r $1 }

prototypes:
    | DATA prototype COMMA prototypes { fun ns r s -> $4 ns r ($2 ns r s) }
    | prototype COMMA prototypes      { fun ns r s -> $3 ns r ($1 ns r s) }
    | DATA prototype                  { $2 }
    | prototype                       { $1 }

prototype:
    | NULL BEGIN body END      { fun ns r s -> $3 r s }
    | reference BEGIN body END { fun ns r s -> $3 r (inherit_proto s ns $1 r) }
    | reference                { fun ns r s -> inherit_proto s ns $1 r }
    | BEGIN body END           { fun ns r s -> $2 r s }

basic:
    | BOOL                  { Bool $1 }
    | INT                   { Num (Int $1) }
    | FLOAT                 { Num (Float $1) }
    | DATA data_reference   { Ref $2 }
    | STRING                { Str $1 }
    | NULL                  { Null }
    | vectors               { Vec $1 }

vectors:
    | LBRACKET items RBRACKET     { $2 }
    | LBRACKET RBRACKET           { [] }

items:
    | basic COMMA items     { $1 :: $3 }
    | basic                 { [] }

link_reference:
    | keywords reference  { LR ($1 ++ $2) }
    | reference           { LR $1 }

data_reference:
    | keywords reference  { ($1 ++ $2) }
    | reference           { $1 }

reference:
    | ANY_ID            { [random_ident 10] }
    | ID SEP reference  { $1 :: $3 }
    | ID                { [$1] }

keywords:
    | ID SEP keywords   { $1 :: $3 }
    | ID                { [$1] }

eos:
	| EOS eos { "" }
	|         { "" }

tag:
	| LBRACKET ID RBRACKET { $2 }
	|                      { "" }

%%
