%{

open Domain

%}

%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID
%token <string> INCLUDE
%token TOK_TBD
%token <string list -> Domain.store -> Domain.store> SF_INCLUDE
%token EXTENDS COMMA DATA BEGIN END SEP NULL LBRACKET RBRACKET EOS EOF

%start sf included          /* entry points: sf -> main file, included -> included file */
%type <Domain.store> sf
%type <string list -> Domain.store -> Domain.store> included

%%
sf:
    | body EOF { $1 [] []
                 (*let root = $1 [] [] in
                 let v = find root ref_main in
                 match v with
                 | Val (Store main) -> (
                     match find (accept main root ref_main replace_link) ref_main with
                     | Val (Store main1) -> main1
                     | _ -> raise (Failure "[err7]") )
                 | _ -> raise (Failure "[err7]") *)
               }

included:
    | body EOF   { $1 }

body:
    | assignment body     { fun ns s -> $2 ns ($1 ns s) }
    | SF_INCLUDE          { $1 }
    |                     { fun ns s -> s }

assignment:
    | reference value  {
                         fun ns s -> let r = $1 in
                                     let (ns1, v1) = resolve s ns (prefix r) in
                                     if (List.length r) = 1 then $2 ns (List.append ns r) s
                                     else match v1 with
                                          | Val (Store s1) -> $2 ns (List.append ns1 r) s
                                          | _ -> raise (Failure "[err6]")
                       }

value:
    | EOS                   { fun ns r s -> bind s r (Basic Null) }
    | TOK_TBD EOS           { fun ns r s -> bind s r TBD }
	| basic EOS             { fun ns r s -> bind s r (Basic $1) }
    | EXTENDS reference BEGIN body END { fun ns r s -> $4 r (inherit_proto (bind s r (Store [])) ns $2 r) }
    | EXTENDS prototypes eos   { fun ns r s -> $2 ns r (bind s r (Store [])) }
    | link_reference EOS    { fun ns r s -> bind s r $1 }

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
    | reference   { LR $1 }

data_reference:
    | reference   { $1 }

reference:
    | ID SEP reference  { $1 :: $3 }
    | ID                { [$1] }

eos:
	| EOS eos { "" }
	|         { "" }

%%
