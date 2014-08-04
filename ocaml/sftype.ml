open Sfsyntax


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

(** alias of functions from module Sfdomain **)
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
let rec domain e r =
	match e with
	| []               -> false
	| (vr, vt) :: tail -> if r = vr then true else (domain e r)

(* return true if type 't' is available in environment 'e', otherwise false *)
let rec has_type e t =
	match t with
	| TUndefined                -> false
	| TVec tv                   -> has_type e tv           (* (Type Vec)    *)
	| TRef tr                   -> has_type e (TBasic tr)  (* (Type Ref)    *)
	| TBasic TBool              -> true                    (* (Type Bool)   *)
	| TBasic TNum               -> true                    (* (Type Num)    *)
	| TBasic TStr               -> true                    (* (Type Str)    *)
	| TBasic TNull              -> true                    (* (Type Null)   *)
	| TBasic TObject            -> true                    (* (Type Object) *)
	| TBasic TAction            -> true                    (* (Type Action) *)
	| TBasic TGlobal            -> true                    (* (Type Global) *)
	| TBasic (TSchema (sid, _)) ->
		match e with
		| []                                        -> false
		| (_, (TBasic (TSchema (side, _)))) :: tail -> if sid = side then true else has_type tail t  (* (Type Schema) *)
		| (_, _) :: tail                            -> has_type tail t

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
		| TBasic (TSchema _), TBasic TObject -> true                      (* (Object Subtype) *)
		| TBasic (TSchema (sid1, super1)), _ -> TBasic super1 <: t2       (* (Trans)          *)
		| TVec tv1, TVec tv2                 -> tv1 <: tv2                (* (Vec Subtype)    *)
		| TRef tr1, TRef tr2                 -> TBasic tr1 <: TBasic tr2  (* (Ref Subtype)    *)
		| TBasic TNull, TRef _               -> true                      (* (Ref Null)       *)
		| _, _ -> false

(**
 * @param e  type environment
 * @param ns namespace where the reference will be resolved
 * @param r  reference to be resolved
 *)
let rec resolve e ns r =
	match r with
	| "ROOT"   :: rs -> ([], type_of e (simplify rs))
	| "PARENT" :: rs -> if ns = [] then failure 2 "PARENT of root namespace is impossible"
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
		| _, Undefined             -> failure 3 ("prototype is not found: " ^ (string_of_ref proto))
		| nsx, Type TBasic TObject -> ref_plus_ref nsx proto
		| _, Type t                -> failure 4 ("invalid prototype: " ^ (string_of_type t))
	in
	let ref_proto = get_proto in
	let e_proto = env_of_ref e ref_proto true in
	List.fold_left (fun ep (rep, tep) -> ((ref_plus_ref r rep), tep) :: ep) e e_proto

let assign e r t t_value =
	match type_of e r with
	| Undefined  ->
		if t = TUndefined then bind e r t_value      (* (Assign1) *)
		else if t_value <: t then bind e r t         (* (Assign3) *)
		else failure 5 "not satisfy rule (Assign3)"
	| Type t_var ->
		if t = TUndefined then
			if t_value <: t_var then e                           (* (Assign2) *)
			else failure 6 "not satisfy rule (Assign2)"
		else if (t_value <: t) && (t <: t_var) then e            (* (Assign4) *)
		else failure 7 "not satisfy rule (Assign2) & (Assign4)"


(*******************************************************************
 * type inference rules
 *******************************************************************)

let sfBoolean b = TBasic TBool  (* (Bool) *)

let sfNumber n = TBasic TNum    (* (Num)  *)

let sfString s = TBasic TStr    (* (Str)  *)

let sfNull = TBasic TNull       (* (Null) *)

let sfReference r = r

let sfDataReference dr =  (* (Deref Data) *)
	fun e ns ->
		match resolve e ns (sfReference dr) with
		| _, Type (TBasic t) -> TRef t
		| _, Type (TRef t)   -> TRef t
		| _, Type (TVec _)   -> failure 101 "dereference of data reference is a vector"
		| _, Type TUndefined -> failure 102 "dereference of data reference is TUndefined"
		| _, Undefined       -> failure 103 "dereference of data reference is undefined"

let sfLinkReference lr =  (* (Deref Link) *)
	fun e ns ->
		match resolve e ns (sfReference lr) with
		| _, Undefined -> failure 104 "dereference of link reference is undefined"
		| _, Type t    -> t

let rec sfVector vec =
	fun e ns ->
		let rec eval v =  (* (Vec) *)
			match v with
			| [] -> TUndefined
			| head :: []   -> sfBasicValue head e ns
			| head :: tail ->
				let t_head = sfBasicValue head e ns in
				if t_head = eval tail then t_head
				else failure 105 "types of vector elements are different"
		in
		TVec (eval vec)

and sfBasicValue bv =
	fun e ns ->
		match bv with
		| Boolean b  -> sfBoolean b
		| Number n   -> sfNumber n
		| String s   -> sfString s
		| Null       -> sfNull
		| Vector vec -> sfVector vec e ns
		| DR dr      -> sfDataReference dr e ns

let rec sfPrototype proto first =
	fun ns r e ->
		match proto with
		| EmptyPrototype ->
			if first then assign e r TUndefined (TBasic TObject)  (* (Proto1) *)
			else e
		| B_P (pb, p)    ->
			let e_block = if first then assign e r TUndefined (TBasic TObject) else e  (* (Proto2) *)
			in
			sfPrototype p false ns r (sfBlock pb r e_block)
		| R_P (pr, p)    ->
			let proto = sfReference pr in
			match resolve e ns proto with
			| _, Undefined    -> failure 106 ("prototype is not found: " ^ (string_of_ref proto))
			| _, Type t_proto ->
				let e_proto = assign e r TUndefined t_proto in             (* (Proto3) & (Proto4) *)
				sfPrototype p false ns r (inherit_env e_proto ns proto r)  (* TODO: Proto3 & Proto4 can be combined (simplified) *)

and sfValue v =
	(**
	 * @param ns namespace
	 * @param t  predefined type
	 * @param e  type environment
	 *)
	fun ns r t e ->
		match v with
		| BV bv   -> assign e r t (sfBasicValue bv e ns)
		| LR link -> assign e r t (sfLinkReference link e ns)
		| P proto -> sfPrototype proto true ns r e
(* sfPrototype proto ns r (bind e r (TBasic TObject)) *)

and sfAssignment (r, t, v) =
	fun ns e -> sfValue v ns (ref_plus_ref ns r) t e

and sfBlock block =
	fun ns e ->
		match block with
		| A_B (a, b) -> sfBlock b ns (sfAssignment a ns e)
		| EmptyBlock -> e

and sfSpecification sf = sfBlock sf [] []



