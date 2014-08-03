(*******************************************************************
 * abstract syntax tree
 *******************************************************************)
type _SF  = _B
and  _B   = AB of _A * _B
          | EB
and  _A   = _R * _V
and  _V   = BV of _BV
          | LR of _LR
          | P of _P
and  _P   = RP of _R * _P
          | BP of _B * _P
          | EP
and  _BV  = Bool of string
          | Num of string
          | Str of string
          | Null
          | Vec of _Vec
          | DR of _DR
and  _LR  = _R
and  _DR  = _R
and  _Vec = _BV list
and  _R   = string list

(*******************************************************************
 * functions to convert elements of abstract syntax tree to string
 *******************************************************************)
let rec string_of_sf sf = string_of_b sf

and string_of_b b =
	match b with
	| AB (a, b) -> (string_of_a a) ^ "\n" ^ (string_of_b b)
	| EB -> ""

and string_of_a a =
	match a with
	| (r, v) -> (string_of_r r) ^ (string_of_v v)

and string_of_v v =
	match v with
	| BV bv -> " " ^ (string_of_bv bv) ^ ";"
	| LR lr -> " " ^ (string_of_r lr) ^ ";"
	| P p -> string_of_p p

and string_of_p p =
	match p with
	| RP (r, p) -> " extends " ^ (string_of_r r) ^ (string_of_p p)
	| BP (b, p) -> " extends {\n" ^ (string_of_b b) ^ "}\n" ^ (string_of_p p)
	| EP -> ""

and string_of_bv bv =
	match bv with
	| Bool b -> b
	| Num n -> n
	| Str s -> s
	| Null -> "NULL"
	| Vec vec -> "[" ^ (string_of_vec vec) ^ "]"
	| DR dr -> "DATA " ^ (string_of_r dr)

and string_of_vec vec =
	match vec with
	| [] -> ""
	| head :: [] -> string_of_bv head
	| head :: tail -> (string_of_bv head) ^ "," ^ (string_of_vec tail)

and string_of_r r = String.concat ":" r
