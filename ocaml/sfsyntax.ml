(*******************************************************************
 * abstract syntax tree
 *******************************************************************)
type sf            = block
and  block         = A_B of assignment * block
                   | EmptyBlock
and  assignment    = reference * _type * value
and  value         = BV  of basicValue
                   | LR  of linkReference
                   | P   of prototype
and  prototype     = R_P of reference * prototype
                   | B_P of block * prototype
                   | EmptyPrototype
and  basicValue    = Boolean of string
                   | Number  of string
                   | String  of string
                   | Null
                   | Vector  of vector
                   | DR      of dataReference
and  linkReference = reference
and  dataReference = reference
and  vector        = basicValue list
and  reference     = string list

(** type syntax **)
and _type     = TBasic of basicType
              | TVec   of _type
              | TRef   of basicType
              | TLazy  of reference * reference * bool  (* ns r [islink] *)
              | TUndefined
and basicType = TBool                         (* (Type Bool)   *)
              | TNum                          (* (Type Num)    *)
              | TStr                          (* (Type Str)    *)
              | TObject                       (* (Type Object) *)
              | TSchema of string * basicType (* (Type Schema) *)
              | TNull                         (* (Type Null)   *)
              | TAction                       (* (Type Action) *)
              | TGlobal                       (* (Type Global) *)

(*******************************************************************
 * functions to convert elements of abstract syntax tree to string
 *******************************************************************)
let rec string_of_sf sf = string_of_b sf

and string_of_b b =
	match b with
	| A_B (a, b) -> (string_of_a a) ^ "\n" ^ (string_of_b b)
	| EmptyBlock -> ""

and string_of_a a =
	match a with
	| (r, t, v) -> (string_of_r r) ^ ":" ^ (string_of_type t) ^ (string_of_v v)

and string_of_v v =
	match v with
	| BV bv -> " " ^ (string_of_bv bv) ^ ";"
	| LR lr -> " " ^ (string_of_r lr) ^ ";"
	| P p   -> string_of_p p

and string_of_p p =
	match p with
	| R_P (r, p)     -> " extends " ^ (string_of_r r) ^ (string_of_p p)
	| B_P (b, p)     -> " extends {\n" ^ (string_of_b b) ^ "}\n" ^ (string_of_p p)
	| EmptyPrototype -> ""

and string_of_bv bv =
	match bv with
	| Boolean b  -> b
	| Number n   -> n
	| String s   -> s
	| Null       -> "NULL"
	| Vector vec -> "[" ^ (string_of_vec vec) ^ "]"
	| DR dr      -> "DATA " ^ (string_of_r dr)

and string_of_vec vec =
	match vec with
	| [] -> ""
	| head :: [] -> string_of_bv head
	| head :: tail -> (string_of_bv head) ^ "," ^ (string_of_vec tail)

and string_of_r r = String.concat ":" r

and string_of_type t =
	match t with
	| TBasic bt         -> string_of_basic_type bt
	| TVec bt           -> "[]" ^ (string_of_type bt)
	| TRef bt           -> "*" ^ (string_of_basic_type bt)
	| TLazy (ns, r, islink) -> "?(" ^ (if islink then "" else "*") ^ (String.concat "." ns) ^ "," ^ (String.concat "." r) ^ ")"
	| TUndefined        -> "!"

and string_of_basic_type t =
	match t with
	| TBool               -> "bool"
	| TNum                -> "num"
	| TStr                -> "str"
	| TObject             -> "obj"
	| TSchema (id, super) -> "$" ^ id ^ "<:" ^ (string_of_basic_type super)
	| TNull               -> "null"
	| TAction             -> "act"
	| TGlobal             -> "glob"
