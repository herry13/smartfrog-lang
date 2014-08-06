(*******************************************************************
 * abstract syntax tree
 *******************************************************************)
type sf            = block
and  sfp           = sfpcontext
and  sfpcontext    = A_C of assignment * sfpcontext
                   | S_C of schema * sfpcontext
                   (* | G_C of global * sfpcontext *)
                   | EmptyContext
and  block         = A_B of assignment * block
                   (* | G_B of global * block *)
                   | EmptyBlock
and  assignment    = reference * _type * value
and  value         = BV  of basicValue
                   | LR  of linkReference
                   | P   of superSchema * prototype
                   (* | A of action *)
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

(** schema syntax **)
and schema      = string * superSchema * block
and superSchema = SID of string
                | EmptySchema

(** type syntax **)
and _type     = TBasic   of basicType
              | TVec     of _type
              | TRef     of basicType
              | TForward of reference * bool  (* r [link: true, data: false] *)
              | TUndefined
and basicType = TBool                         (* (Type Bool)   *)
              | TNum                          (* (Type Num)    *)
              | TStr                          (* (Type Str)    *)
              | TObject                       (* (Type Object) *)
              | TSchema of string * basicType (* (Type Schema) *)
              | TNull                         (* (Type Null)   *)
              | TAction                       (* (Type Action) *)
              | TGlobal                       (* (Type Global) *)
              | TRootSchema

(*******************************************************************
 * functions to convert elements of abstract syntax tree to string
 *******************************************************************)
let rec string_of_sf sf = string_of_block sf

and string_of_block b =
	match b with
	| A_B (a, b) -> (string_of_assignment a) ^ "\n" ^ (string_of_block b)
	| EmptyBlock -> ""

and string_of_assignment a =
	match a with
	| (r, t, v) -> (string_of_ref r) ^ ":" ^ (string_of_type t) ^ (string_of_value v)

and string_of_value v =
	match v with
	| BV bv      -> " " ^ (string_of_bv bv) ^ ";"
	| LR lr      -> " " ^ (string_of_ref lr) ^ ";"
	| P (sid, p) -> (string_of_super_schema sid) ^ (string_of_proto p)

and string_of_proto p =
	match p with
	| R_P (r, p)     -> " extends " ^ (string_of_ref r) ^ (string_of_proto p)
	| B_P (b, p)     -> " extends {\n" ^ (string_of_block b) ^ "}\n" ^ (string_of_proto p)
	| EmptyPrototype -> ""

and string_of_bv bv =
	match bv with
	| Boolean b  -> b
	| Number n   -> n
	| String s   -> s
	| Null       -> "NULL"
	| Vector vec -> "[" ^ (string_of_vec vec) ^ "]"
	| DR dr      -> "DATA " ^ (string_of_ref dr)

and string_of_vec vec =
	match vec with
	| [] -> ""
	| head :: [] -> string_of_bv head
	| head :: tail -> (string_of_bv head) ^ "," ^ (string_of_vec tail)

and string_of_ref r = String.concat ":" r

and string_of_type t =
	match t with
	| TBasic bt            -> string_of_basic_type bt
	| TVec t               -> "[]" ^ (string_of_type t)
	| TRef bt              -> "*" ^ (string_of_basic_type bt)
	| TForward (r, islink) -> "?(" ^ (if islink then "" else "*") ^ (String.concat "." r) ^ ")"
	| TUndefined           -> "!"

and string_of_basic_type t =
	match t with
	| TBool               -> "bool"
	| TNum                -> "num"
	| TStr                -> "str"
	| TObject             -> "obj"
	| TSchema (id, super) -> "$" ^ id ^ "<:" ^ (string_of_basic_type super)
	| TRootSchema         -> "$$"
	| TNull               -> "null"
	| TAction             -> "act"
	| TGlobal             -> "glob"

and string_of_super_schema sid =
	match sid with
	| SID id      -> " isa " ^ id
	| EmptySchema -> ""

and string_of_schema (sid, ss, b) =
	sid ^ (string_of_super_schema ss) ^ " {\n" ^ (string_of_block b) ^ "}"

and string_of_sfpcontext ctx =
	match ctx with
	| A_C (a, c)   -> (string_of_assignment a) ^ "\n" ^ (string_of_sfpcontext c)
	| S_C (s, c)   -> (string_of_schema s) ^ "\n" ^ (string_of_sfpcontext c)
	| EmptyContext -> ""

and string_of_sfp sfp = string_of_sfpcontext sfp
