%{
    open Domain
%}
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID
%token EXTENDS COMMA BEGIN END SEP NULL EOS EOF
%start sf           /* entry point */
%type <Domain.store> sf
%%
sf:
    body EOF            { $1 [] [] }
;
body:
    | assignment body   { fun ns s -> $2 ns ($1 ns s) }
    |                   { fun ns s -> s }
;
assignment:
    reference value    { fun ns s -> $2 ns (List.append ns $1) s }
;
value:
	  basic EOS          { fun ns r s -> bind s r $1 }
    | EXTENDS prototypes { fun ns r s -> $2 ns r (bind s r (Store [])) }
;
prototypes:
      prototype COMMA prototypes { fun ns r s -> $3 ns r ($1 ns r s) }
    | prototype                  { $1 }
;
prototype:
      BEGIN body END    { fun ns r s -> $2 r s }
;
basic:
      BOOL      { Bool $1 }
    | INT       { Num (Int $1) }
    | FLOAT     { Num (Float $1) }
    | STRING    { Str (String.sub $1 1 ((String.length $1)-2)) }
    | NULL      { Null }
;
reference:
      ID SEP reference  { $1 :: $3 }
    | ID                { [$1] }
;
