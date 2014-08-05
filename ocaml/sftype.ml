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

let failure code msg = raise (Failure ("[type-err" ^ string_of_int(code) ^ "] " ^ msg))

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
let ref_prefixeq_ref = Sfdomain.ref_prefixeq_ref;;
let ref_prefix_ref = Sfdomain.ref_prefix_ref;;
let simplify = Sfdomain.simplify;;
let trace = Sfdomain.trace;;
module SetRef = Sfdomain.SetRef

(*******************************************************************
 * type assignment rules
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
	| (vr, vt) :: tail -> if r = vr then true else (domain tail r)

(* return true if type 't' is available in environment 'e', otherwise false *)
let rec has_type e t =
	match t with
	| TUndefined                -> false
	| TForward (r, islink)         -> true
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

(* bind type 't' to variable 'r' into environment 'e' *)
let bind e r t =
	if (type_of e r) != Undefined then failure 1 ("cannot bind type to an existing variable " ^ (string_of_ref r))
	else if (List.length r) > 1 then
		match type_of e (prefix r) with
		| Undefined     -> failure 2 ("prefix of " ^ (string_of_ref r) ^ " is not defined")
		| Type t_prefix ->
			if t_prefix <: TBasic TObject then (r, t) :: e
			else failure 3 ("prefix of " ^ (string_of_ref r) ^ " is not a component")
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

(**
 * @param e  type environment
 * @param ns namespace where the reference will be resolved
 * @param r  reference to be resolved
 *)
let rec resolve e ns r =
	match r with
	| "ROOT"   :: rs -> ([], type_of e (simplify rs))
	| "PARENT" :: rs -> if ns = [] then failure 4 "PARENT of root namespace is impossible"
	                    else (prefix ns, type_of e (simplify (ref_plus_ref (prefix ns) rs)))
	| "THIS"   :: rs -> (ns, type_of e (simplify (ref_plus_ref ns rs)))
	| _              ->
		if ns = [] then ([], type_of e r)
		else
			let t = type_of e (trace ns r) in
			if t = Undefined then resolve e (prefix ns) r
			else (ns, t)

let copy e proto dest =
	let e_proto = env_of_ref e proto true in
	List.fold_left (fun ep (rep, tep) -> ((ref_plus_ref dest rep), tep) :: ep) e e_proto

(**
 * @param e     type environment
 * @param ns    namespace where references will be resolved
 * @param proto reference of prototype
 * @param r     target variable
 *)
let inherit_env e ns proto r =
	let get_proto =
		match resolve e ns proto with
		| _, Undefined             -> failure 5 ("prototype is not found: " ^ (string_of_ref proto))
		| nsx, Type TBasic TObject -> ref_plus_ref nsx proto
		| _, Type t                -> failure 6 ("invalid prototype " ^ (string_of_ref r) ^ ":" ^ (string_of_type t))
	in
	copy e get_proto r

(**
 * @param e       type environment
 * @param r       reference of the variable
 * @param t       pre-defined type
 * @param t_value type of value which will be assigned
 *)
let assign e r t t_value =
	match type_of e r with
	| Undefined  ->
		if t = TUndefined then bind e r t_value      (* (Assign1) *)
		else if t_value <: t then bind e r t         (* (Assign3) *)
		else failure 7 "not satisfy rule (Assign3)"
	| Type t_var ->
		if t = TUndefined then
			if t_value <: t_var then e                           (* (Assign2) *)
			else failure 8 "not satisfy rule (Assign2) "
		else if (t_value <: t) && (t <: t_var) then e            (* (Assign4) *)
		else failure 9 "not satisfy rule (Assign2) & (Assign4)"

(* resolve forward type assignment for lazy & data reference *)
let rec resolve_tlazy e ns r acc =
	let follow_tlazy nsp tr =
		let rp = ref_plus_ref nsp r in
		if ref_prefixeq_ref rp tr then failure 11 ("implicit cyclic reference " ^ (string_of_ref tr))
		else resolve_tlazy e rp tr (SetRef.add rp acc)
	in
	if SetRef.exists (fun rx -> rx = r) acc then failure 11 ("cyclic reference " ^ (string_of_ref r))
	else
		match resolve e ns r with
		| _, Undefined                    -> failure 10 ("undefined reference " ^ (string_of_ref r) ^ " in " ^ (string_of_ref ns))
		| nsp, Type TForward (tr, islink) -> follow_tlazy nsp tr
		| nsp, Type t -> ((ref_plus_ref nsp r), t)

let resolve_tforward_of_env e =
	let sfconfig = ["sfConfig"] in
	let rec iter e src =
		let replace_tforward r t tr islink =
			let (proto, t_val) =
				match resolve_tlazy e r tr SetRef.empty with
				| proto, TBasic t -> (proto, (if islink then TBasic t else TRef t))
				| proto, t        -> (proto, t)
			in
			let ex = (r, t_val) :: e in
			if t_val <: TBasic TObject then copy ex proto r
			else ex
		in
		match src with
		| [] -> e
		| (r, t) :: tail ->
			if not (ref_prefixeq_ref sfconfig r) || r = sfconfig then iter e tail
			else
				let result =
					match t with
					| TForward (tr, islink) -> replace_tforward r t tr islink
					| _  -> (r, t) :: e
				in
				iter result tail
	in
	iter e e

let second_pass_of_env e = resolve_tforward_of_env e

let get_sfconfig e =
	let sfconfig = ["sfConfig"] in
	let rec iter e buf =
		match e with
		| [] -> buf
		| (r, t) :: tail ->
			if not (ref_prefixeq_ref sfconfig r) then iter tail buf
			else
				let r1 = List.tl r in
				if r1 = [] || domain buf r1 then iter tail buf else iter tail ((r1, t) :: buf)
	in
	iter e []

(*******************************************************************
 * type inference rules
 *******************************************************************)

let sfBoolean b = TBasic TBool  (* (Bool) *)

let sfNumber n = TBasic TNum    (* (Num)  *)

let sfString s = TBasic TStr    (* (Str)  *)

let sfNull = TBasic TNull       (* (Null) *)

let sfReference r = r

let sfDataReference dr =  (* (Deref Data) *)
	(**
	 * @param ns namespace
	 * @param e  type environment
	 *)
	fun e ns ->
		let r = (sfReference dr) in
		match resolve e ns r with
		| _, Type (TForward (rz, islink)) -> TForward (rz, islink)
		| _, Type (TBasic t) -> TRef t
		| _, Type (TRef t)   -> TRef t
		| _, Type (TVec _)   -> failure 101 ("dereference of " ^ (string_of_ref r) ^ " is a vector")
		| _, Type TUndefined -> failure 102 ("dereference of " ^ (string_of_ref r) ^ " is TUndefined")
		| _, Undefined       -> TForward (r, false)

let sfLinkReference lr =  (* (Deref Link) *)
	(**
	 * @param ns namespace
	 * @param e  type environment
	 *)
	fun e ns r ->
		let link = sfReference lr in
		match resolve e ns link with
		| nsp, Undefined -> ((ref_plus_ref nsp link), TForward (link, true))
		| nsp, Type t  -> ((ref_plus_ref nsp link), t)

let rec sfVector vec =
	(**
	 * @param ns namespace
	 * @param e  type environment
	 *)
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
	(**
	 * @param ns namespace
	 * @param e  type environment
	 *)
	fun e ns ->
		match bv with
		| Boolean b  -> sfBoolean b
		| Number n   -> sfNumber n
		| String s   -> sfString s
		| Null       -> sfNull
		| Vector vec -> sfVector vec e ns
		| DR dr      -> sfDataReference dr e ns

(**
 * @param proto prototype AST element
 * @param first true if this is the first prototype, otherwise false
 * @param t_val the type of the first prototype
 *)
let rec sfPrototype proto first t_val =
	(**
	 * @param ns namespace
	 * @param r  target variable
	 * @param e  type environment
	 *)
	fun ns r e ->
		match proto with
		| EmptyPrototype ->
			if first then assign e r t_val (TBasic TObject)  (* (Proto1) *)
			else e
		| B_P (pb, p)    ->
			let e_block = if first then assign e r t_val (TBasic TObject) else e  (* (Proto2) *)
			in
			let t_block = if first then TBasic TObject else t_val
			in
			sfPrototype p false t_block ns r (sfBlock pb r e_block)
		| R_P (pr, p)    ->
			let proto = sfReference pr in
			match resolve e ns proto with
			| _, Undefined -> failure 106 ("prototype is not found: " ^ (string_of_ref proto))
			| _, Type t    ->
				let e_proto = assign e r t_val t in                      (* (Proto3) & (Proto4) *)
				let t_proto = if first then t else t_val in
				sfPrototype p false t_proto ns r (inherit_env e_proto ns proto r)

and sfValue v =
	(**
	 * @param ns namespace
	 * @param t  predefined type
	 * @param e  type environment
	 *)
	fun ns r t e ->
		match v with
		| BV bv   -> assign e r t (sfBasicValue bv e ns)
		| LR link ->
			let (r_link, t_link) = sfLinkReference link e ns r in
			let e1 = assign e r t t_link in
			if t_link <: TBasic TObject then copy e1 r_link r
			else e1
		| P proto -> sfPrototype proto true TUndefined ns r e

and sfAssignment (r, t, v) =
	(**
	 * @param ns namespace
	 * @param e  type environment
	 *)
	fun ns e -> sfValue v ns (ref_plus_ref ns r) t e

and sfBlock block =
	(**
	 * @param ns namespace
	 * @param e  type environment
	 *)
	fun ns e ->
		match block with
		| A_B (a, b) -> sfBlock b ns (sfAssignment a ns e)
		| EmptyBlock -> e

and sfSpecification sf =
	let e1 = sfBlock sf [] [] in
	if not (domain e1 ["sfConfig"]) then failure 107 "sfConfig is not exist"
	else get_sfconfig (second_pass_of_env e1)

