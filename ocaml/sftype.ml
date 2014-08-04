open Sfast


(*******************************************************************
 * type environment
 *******************************************************************)

type _t = Type of _type
        | Undefined
and var = string list * _type
and env = var list


(*******************************************************************
 * helper functions
 *******************************************************************)

let failure code msg = raise (Failure ("[err" ^ string_of_int(code) ^ "] " ^ msg))

let string_of_ref r = String.concat "." r

let string_of_env e =
	let rec str_of_env e s =
		match e with
		| (r1, t1) :: tail ->
			let buf = (string_of_ref r1) ^ " : " ^ (string_of_type t1) in
			if tail = [] then s ^ buf
			else str_of_env tail (s ^ buf ^ "\n")
		| _ -> s
	in
	str_of_env e ""

(** alias functions from module Sfdomain **)
let ref_plus_ref = Sfdomain.ref_plus_ref;;
let ref_minus_ref = Sfdomain.ref_minus_ref;;
let prefix = Sfdomain.prefix;;
let ref_prefix_ref = Sfdomain.ref_prefix_ref;;
let simplify = Sfdomain.simplify;;
let trace = Sfdomain.trace;;

(*******************************************************************
 * type checking rules
 *******************************************************************)

(** return the type of variable 'r' **)
let rec type_of e r =
	match e with
	| []               -> Undefined
	| (vr, vt) :: tail -> if r = vr then Type vt else type_of tail r

(** return true if variable 'r' is in environment 'e', otherwise false *)
let rec dom e r =
	match e with
	| []               -> false
	| (vr, vt) :: tail -> if r = vr then true else (dom e r)

(* return true if type 't' is available in environment 'e', otherwise false *)
let rec has_type e t =
	match t with
	| Tundefined                -> false
	| Tvec tv                   -> has_type e tv           (* (Type Vec)    *)
	| Tref tr                   -> has_type e (Tbasic tr)  (* (Type Ref)    *)
	| Tbasic Tbool              -> true                    (* (Type Bool)   *)
	| Tbasic Tnum               -> true                    (* (Type Num)    *)
	| Tbasic Tstr               -> true                    (* (Type Str)    *)
	| Tbasic Tnull              -> true                    (* (Type Null)   *)
	| Tbasic Tobj               -> true                    (* (Type Object) *)
	| Tbasic Tact               -> true                    (* (Type Action) *)
	| Tbasic Tglob              -> true                    (* (Type Global) *)
	| Tbasic (Tschema (sid, _)) ->
		match e with
		| []                                                    -> false
		| (_, (Tbasic (Tschema (side, _)))) :: tail -> if sid = side then true else has_type tail t  (* (Type Schema) *)
		| (_, _) :: tail                                        -> has_type tail t

(* bind type 't' to variable 'r' into environment 'e' *)
let bind e r t =
	if (type_of e r) != Undefined then failure 1 ("cannot bind an existing variable " ^ (string_of_ref r))
	else (r, t) :: e

(**
 * return part of environment 'env' where 'ref' is the prefix of the variables
 * the prefix of variables will be removed when 'cut' is true, otherwise
 * the variables will have original references
 *)
let rec env_of_ref env ref cut =
	if ref = [] then env
	else if env = [] then []
	else
		List.fold_left
		(
			fun e (r, t) ->
				if ref_prefix_ref ref r then
					let re = if cut then (ref_minus_ref r ref) else r in
					(re, t) :: e
				else e
		) [] env

(* return true if type 't1' is a sub-type of 't2', otherwise false *)
let rec (<:) t1 t2 =
	if t1 = t2 then true                                                  (* (Reflex)         *)
	else
		match t1, t2 with
		| Tbasic (Tschema _), Tbasic Tobj    -> true                      (* (Object Subtype) *)
		| Tbasic (Tschema (sid1, super1)), _ -> Tbasic super1 <: t2       (* (Trans)          *)
		| Tvec tv1, Tvec tv2                 -> tv1 <: tv2                (* (Vec Subtype)    *)
		| Tref tr1, Tref tr2                 -> Tbasic tr1 <: Tbasic tr2  (* (Ref Subtype)    *)
		| Tbasic Tnull, Tref _               -> true                      (* (Ref Null)       *)
		| _, _ -> false


(*******************************************************************
 * type inference rules
 *******************************************************************)

let sfBoolean b = Tbasic Tbool

let sfNumber n = Tbasic Tnum

let sfString s = Tbasic Tstr

let sfNull = Tbasic Tnull

let sfReference r = r

let sfDataReference dr =
	fun e ->
		match type_of e (sfReference dr) with
		| Type (Tbasic t) -> Tref t
		| Type (Tref t)   -> Tref t
		| Type (Tvec _)   -> failure 101 "dereference of data reference is a vector"
		| Type Tundefined -> failure 102 "dereference of data reference is Tundefined"
		| Undefined       -> failure 103 "dereference of data reference is undefined"

let sfLinkReference lr =
	fun e ->
		match type_of e (sfReference lr) with
		| Undefined -> failure 104 "dereference of link reference is undefined"
		| Type t    -> t

let rec sfVector vec =
	fun e ->
		let rec eval v =
			match v with
			| [] -> Tundefined
			| head :: [] -> sfBasicValue head e
			| head :: tail ->
				let t_head = sfBasicValue head e in
				if t_head = eval tail then t_head
				else failure 105 "types of vector elements are different"
		in
		Tvec (eval vec)

and sfBasicValue bv =
	fun e ->
		match bv with
		| Boolean b  -> sfBoolean b
		| Number n   -> sfNumber n
		| String s   -> sfString s
		| Null       -> sfNull
		| Vector vec -> sfVector vec e
		| DR dr      -> sfDataReference dr e

let rec resolve e ns r =
	match r with
	| "ROOT"   :: rs -> ([], type_of e (simplify rs))
	| "PARENT" :: rs -> if ns = [] then failure 10 "PARENT of root namespace is impossible"
	                    else (prefix ns, type_of e (simplify (ref_plus_ref (prefix ns) rs)))
	| "THIS"   :: rs -> (ns, type_of e (simplify (ref_plus_ref ns rs)))
	| _              ->
		if ns = [] then ([], type_of e r)
		else
			let t = type_of e (trace ns r) in
			match t with
			| Undefined -> resolve e (prefix ns) r
			| _ -> (ns, t)

let inherit_env e ns proto r =
	let get_proto =
		match resolve e ns proto with
		| _, Undefined          -> failure 12 ("prototype is not found: " ^ (string_of_ref proto))
		| nsx, Type Tbasic Tobj -> ref_plus_ref nsx proto
		| _, Type t             -> failure 13 ("invalid prototype: " ^ (string_of_type t))
	in
	let ref_proto = get_proto in
	let e_proto = env_of_ref e ref_proto true in
	List.fold_left (fun ep (rep, tep) -> ((ref_plus_ref r rep), tep) :: ep) e e_proto

let rec sfPrototype proto =
	fun ns r e ->
		match proto with
		| B_P (pb, p)    -> sfPrototype p ns r (sfBlock pb r e)
		| R_P (pr, p)    -> sfPrototype p ns r (inherit_env e ns (sfReference pr) r)
		| EmptyPrototype -> e

and sfValue v =
	fun ns r e ->
		match v with
		| BV bv   -> bind e r (sfBasicValue bv e)
		| LR link -> bind e r (sfLinkReference link e)
		| P proto -> sfPrototype proto ns r (bind e r (Tbasic Tobj))

and sfAssignment (r, t, v) =
	fun ns e -> sfValue v ns (ref_plus_ref ns r) e

and sfBlock block =
	fun ns e ->
		match block with
		| A_B (a, b) -> sfBlock b ns (sfAssignment a ns e)
		| EmptyBlock -> e

and sfSpecification sf = sfBlock sf [] []



