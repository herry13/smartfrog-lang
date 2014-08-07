(*******************************************************************
 * abstract syntax tree
 *******************************************************************)
type sf            = block
and  sfp           = sfpcontext
and  sfpcontext    = A_C of assignment * sfpcontext
                   | S_C of schema * sfpcontext
                   | G_C of global * sfpcontext
                   | EmptyContext
and  block         = A_B of assignment * block
                   | G_B of global * block
                   | EmptyBlock
and  assignment    = reference * _type * value
and  value         = BV  of basicValue
                   | LR  of linkReference
                   | P   of superSchema * prototype
                   | Ac  of action
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

(** constraint syntax **)
and global      = _constraint
and _constraint = Eq of equal
                | Ne of notEqual
                | Not of negation
                | Imply of implication
                | And of conjunction
                | Or of disjunction
                | In of membership
and equal       = reference * basicValue
and notEqual    = reference * basicValue
and implication = _constraint * _constraint
and negation    = _constraint
and membership  = reference * vector
and conjunction = _constraint list
and disjunction = _constraint list

(** action syntax **)
and action = parameters * cost * conditions * effects
and parameters = parameter list
and parameter = string * _type
and cost = Cost of string | EmptyCost
and conditions = Cond of _constraint | EmptyCondition
and effects = effect list
and effect = reference * basicValue

(*******************************************************************
 * functions to convert elements of abstract syntax tree to string
 *******************************************************************)
let rec string_of_sf sf = string_of_block sf

and string_of_block b =
	match b with
	| A_B (a, b) -> (string_of_assignment a) ^ "\n" ^ (string_of_block b)
	| G_B (g, b) -> (string_of_global g) ^ "\n" ^ (string_of_block b)
	| EmptyBlock -> ""

and string_of_assignment a =
	match a with
	| (r, t, v) -> (string_of_ref r) ^ ":" ^ (string_of_type t) ^ (string_of_value v)

and string_of_value v =
	match v with
	| BV bv      -> " " ^ (string_of_basic_value bv) ^ ";"
	| LR lr      -> " " ^ (string_of_ref lr) ^ ";"
	| P (sid, p) -> (string_of_super_schema sid) ^ (string_of_proto p)
	| Ac a       -> string_of_action a

and string_of_proto p =
	match p with
	| R_P (r, p)     -> " extends " ^ (string_of_ref r) ^ (string_of_proto p)
	| B_P (b, p)     -> " extends {\n" ^ (string_of_block b) ^ "}\n" ^ (string_of_proto p)
	| EmptyPrototype -> ""

and string_of_basic_value bv =
	match bv with
	| Boolean b  -> b
	| Number n   -> n
	| String s   -> s
	| Null       -> "NULL"
	| Vector vec -> "[" ^ (string_of_vector vec) ^ "]"
	| DR dr      -> "DATA " ^ (string_of_ref dr)

and string_of_vector vec =
	match vec with
	| [] -> ""
	| head :: [] -> string_of_basic_value head
	| head :: tail -> (string_of_basic_value head) ^ "," ^ (string_of_vector tail)

and string_of_ref r = String.concat ":" r

and (!^) r = string_of_ref r

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
	| G_C (g, c)   -> (string_of_global g) ^ "\n" ^ (string_of_sfpcontext c)
	| EmptyContext -> ""

and string_of_sfp sfp = string_of_sfpcontext sfp

(** constraints *)
and string_of_global g =
	"global " ^ (string_of_constraint g) ^ "\n"

and string_of_conjunction cs =
	(List.fold_left (fun s c -> s ^ " " ^ (string_of_constraint c)) "(and " cs) ^ ")"

and string_of_disjunction cs =
	(List.fold_left (fun s c -> s ^ " " ^ (string_of_constraint c)) "(or " cs) ^ ")"

and string_of_constraint c =
	match c with
	| Eq e    -> string_of_equal e
	| Ne e    -> string_of_not_equal e
	| Not e   -> string_of_negation e
	| Imply e -> string_of_implication e
	| And e   -> string_of_conjunction e
	| Or e    -> string_of_conjunction e
	| In e    -> string_of_membership e

and string_of_equal (r, bv) =
	"(= " ^ !^r ^ " " ^ (string_of_basic_value bv) ^ ")"

and string_of_not_equal (r, bv) =
	"(!= " ^ !^r ^ " " ^ (string_of_basic_value bv) ^ ")"

and string_of_negation e =
	"(not " ^ (string_of_constraint e) ^ ")"

and string_of_implication (e1, e2) =
	"(imply " ^ (string_of_constraint e1) ^ " " ^ (string_of_constraint e2) ^ ")"

and string_of_membership (r, v) =
	"(in " ^ !^r ^ " " ^ (string_of_vector v) ^ ")"

(** action **)
and string_of_effect (r, bv) =
	"(= " ^ !^r ^ " " ^ (string_of_basic_value bv) ^ ")"

and string_of_effects effs =
	(List.fold_left (fun s e -> s ^ " " ^ (string_of_effect e)) "(effects " effs) ^ ")"

and string_of_conditions cond =
	match cond with
	| EmptyCondition -> ""
	| Cond c         -> "(conditions " ^ (string_of_constraint c) ^ ")"

and string_of_cost cost =
	match cost with
	| EmptyCost -> ""
	| Cost c    -> "(cost " ^ c ^ ")"

and string_of_parameter (id, t) = "(= " ^ id ^ " " ^ (string_of_type t) ^ ")"

and string_of_parameters params =
	(List.fold_left (fun s p -> s ^ " " ^ (string_of_parameter p)) "(params " params) ^ ")"

and string_of_action (params, cost, conds, effs) =
	"(action " ^ (string_of_parameters params) ^ " " ^ (string_of_conditions conds) ^ " " ^ (string_of_effects effs) ^ ")"
