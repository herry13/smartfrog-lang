%{
    open Domain
%}
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID
%token EXTENDS COMMA DATA BEGIN END SEP NULL LBRACKET RBRACKET EOS EOF
%start sf           /* entry point */
%type <Domain.store> sf
%%
sf:
    body                {
                          let r = ["sfConfig"] in
                          let s = $1 [] [] in
                          let v = find s r in
                          match v with
                          | Val (Store s) -> s
                          | _ -> raise (Failure "[err7]")
                        }
;
body:
    | assignment body   { fun ns s -> $2 ns ($1 ns s) }
    |                   { fun ns s -> s }
;
assignment:
    reference value    {
                         fun ns s -> let r = $1 in
                                     let (ns1, v1) = resolve s ns (prefix r) in
                                     if (List.length r) = 1 then $2 ns (List.append ns r) s
                                     else match v1 with
                                          | Val (Store s1) -> $2 ns (List.append ns1 r) s
                                          | _ -> raise (Failure "[err6]")
                       }
;
value:
	  basic EOS             { fun ns r s -> bind s r $1 }
    | EXTENDS prototypes    { fun ns r s -> $2 ns r (bind s r (Store [])) }
    | link_reference EOS    {
                              fun ns r s -> let (_, v1) = resolve s ns $1 in
                                            match v1 with
                                            | Undefined -> raise (Failure "[err5]")
                                            | Val v -> bind s r v
                            }
;
prototypes:
      prototype COMMA prototypes { fun ns r s -> $3 ns r ($1 ns r s) }
    | prototype                  { $1 }
;
prototype:
      BEGIN body END    { fun ns r s -> $2 r s }
    | reference         { fun ns r s -> inherit_proto s ns $1 r }
;
basic:
      BOOL                  { Bool $1 }
    | INT                   { Num (Int $1) }
    | FLOAT                 { Num (Float $1) }
    | DATA data_reference   { Ref $2 }
    | STRING                { Str (String.sub $1 1 ((String.length $1)-2)) }
    | NULL                  { Null }
    | vectors               { Vec $1 }
;
vectors:
    LBRACKET items RBRACKET     { $2 }
;
items:
      basic COMMA items     { $1 :: $3 }
    | basic                 { [$1] }
;
link_reference:
    reference   { $1 }
;
data_reference:
    reference   { $1 }
;
reference:
      ID SEP reference  { $1 :: $3 }
    | ID                { [$1] }
;
