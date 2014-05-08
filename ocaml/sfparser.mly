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
%token MERGE EXTENDS COMMA DATA BEGIN END SEP NULL LBRACKET RBRACKET EOS EOF

%start sf included          /* entry points: sf -> main file, included -> included file */
%type <Domain.store> sf
%type <string list -> Domain.store -> Domain.store> included

%%
sf:
    | body EOF {
                 let r = ["sfConfig"] in
                 let s = $1 [] [] in
                 let v = find s r in
                 match v with
                 | Val (Store main) -> main
                 | _ -> raise (Failure "[err7]")
               }

included:
    | body EOF   { $1 }

body:
    | assignment body     { fun ns s -> $2 ns ($1 ns s) }
    | SF_INCLUDE          { $1 }
    |                     { fun ns s -> s }

assignment:
    | MERGE reference value {
                              fun ns s -> let (ns1, v1) = resolve s ns $2 in
                                          match v1 with
                                          | Undefined -> raise (Failure "[err9] cannot merge with undefined value")
                                          | _ -> $3 ns (List.append ns1 $2) s
                            }
    | reference value       { fun ns s -> $2 ns (List.append ns $1) s }

value:
	| basic EOS             { fun ns r s -> bind s r (Basic $1) }
    | EXTENDS prototypes    { fun ns r s -> $2 ns r (bind s r (Store [])) }
    | link_reference EOS    {
                              fun ns r s -> let (_, v1) = resolve s ns $1 in
                                            match v1 with
                                            | Undefined -> raise (Failure "[err5]")
                                            | Val v -> bind s r v
                            }
    | BEGIN body END        { fun ns r s -> $2 r (bind s r (Store [])) (* syntactic sugar *) }

prototypes:
    | prototype COMMA prototypes { fun ns r s -> $3 ns r ($1 ns r s) }
    | prototype                  { $1 }

prototype:
    | BEGIN body END    { fun ns r s -> $2 r s }
    | reference         { fun ns r s -> inherit_proto s ns $1 r }

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

items:
    | basic COMMA items     { $1 :: $3 }
    | basic                 { [$1] }

link_reference:
    | reference   { $1 }

data_reference:
    | reference   { $1 }

reference:
    | ID SEP reference  { $1 :: $3 }
    | ID                { [$1] }

%%
