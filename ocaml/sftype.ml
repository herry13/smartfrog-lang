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

exception TypeError of int * string

let error code msg = raise (TypeError (code, "[type-err" ^ string_of_int(code) ^ "] " ^ msg))

let (!^) r = String.concat "." r

let string_of_env e =
	let rec str_of_env e s =
		match e with
		| (r1, t1) :: tail ->
			let buf = (!^r1) ^ " : " ^ (string_of_type t1) in
			if tail = [] then s ^ buf
			else str_of_env tail (s ^ buf ^ "\n")
		| _ -> s
	in
	str_of_env e ""

(** alias of functions from module Sfdomain **)
let (@++)  = Sfdomain.ref_plus_ref;;
let (@--)  = Sfdomain.ref_minus_ref;;
let prefix = Sfdomain.prefix;;
let (@<=)  = Sfdomain.ref_prefixeq_ref;;
let (@<)   = Sfdomain.ref_prefix_ref;;
let (!!)   = Sfdomain.simplify;;
let (@<<)  = Sfdomain.trace;;
module SetRef = Sfdomain.SetRef

(*******************************************************************
 * typing judgement functions
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

(* return true if type 't1' is a sub-type of 't2', otherwise false *)
let rec (<:) t1 t2 =                                                      (* Subtyping rules  *)
	if t1 = t2 then true                                                  (* (Reflex)         *)
	else
		match t1, t2 with
		| TBasic (TSchema _), TBasic TObject -> true                      (* (Object Subtype) *)
		| TBasic (TSchema (sid1, super1)), _ -> TBasic super1 <: t2       (* (Trans)          *)
		| TVec tv1, TVec tv2                 -> tv1 <: tv2                (* (Vec Subtype)    *)
		| TRef tr1, TRef tr2                 -> TBasic tr1 <: TBasic tr2  (* (Ref Subtype)    *)
		| TBasic TNull, TRef _               -> true                      (* (Ref Null)       *)
		| _, _ -> false


(*******************************************************************
 * well-formed environments and types functions
 *******************************************************************)

(* return true if type 't' is available in environment 'e', otherwise false *)
let rec has_type e t =
	match t with
	| TUndefined                -> false
	| TForward (r, islink)      -> true                    (* TODO (Type Forward) *)
	| TVec tv                   -> has_type e tv           (* (Type Vec)    *)
	| TRef tr                   -> has_type e (TBasic tr)  (* (Type Ref)    *)
	| TBasic TBool              -> true                    (* (Type Bool)   *)
	| TBasic TNum               -> true                    (* (Type Num)    *)
	| TBasic TStr               -> true                    (* (Type Str)    *)
	| TBasic TNull              -> true                    (* (Type Null)   *)
	| TBasic TObject            -> true                    (* (Type Object) *)
	| TBasic TAction            -> true                    (* (Type Action) *)
	| TBasic TGlobal            -> true                    (* (Type Global) *)
	| TBasic TRootSchema        -> true
	| TBasic (TSchema (sid, _)) ->
		match e with
		| []                                        -> false
		| (_, (TBasic (TSchema (side, _)))) :: tail -> if sid = side then true  (* (Type Schema) *)
		                                               else has_type tail t
		| (_, _) :: tail                            -> has_type tail t

let is_schema t = t <: (TBasic TRootSchema)

(*******************************************************************
 * type assignment functions
 *******************************************************************)

(* bind type 't' to variable 'r' into environment 'e' *)
let bind e r t =
	if (type_of e r) != Undefined then error 1 ("cannot bind type to an existing variable " ^ !^r)
	else if (List.length r) > 1 then
		match type_of e (prefix r) with
		| Undefined     -> error 2 ("prefix of " ^ !^r ^ " is not defined")
		| Type t_prefix ->
			if t_prefix <: TBasic TObject then (r, t) :: e
			else error 3 ("prefix of " ^ !^r ^ " is not a component")
	else (r, t) :: e

(**
 * @param e       type environment
 * @param r       reference of the variable
 * @param t       pre-defined type
 * @param t_value type of value which will be assigned
 *)
let assign e r t t_value =
	match (type_of e r), t, t_value with
	| Undefined, TUndefined, _                             -> bind e r t_value                          (* (Assign1) *)
	| Undefined, t, _ when t_value <: t                    -> bind e r t                                (* (Assign3) *)
	| Undefined, t, TForward (r, islink)                   -> (r, t) :: (r, TForward (r, islink)) :: e  (* TODO (Assign5) *)
	| Undefined, _, _                                      -> error 7 "not satisfy rule (Assign3)"
	| Type (TForward (r, islink)), _, _                    -> (r, t) :: (r, t_value) :: e
	| Type t_var, TUndefined, _ when t_value <: t_var      -> e                                         (* (Assign2) *)
	| Type t_var, TUndefined, TForward (r, islink)         -> (r, TForward (r, islink)) :: e            (* TODO (Assign6) *)
	| Type t_var, TUndefined, _                            -> error 8 "not satisfy rule (Assign2)"
	| Type t_var, _, _ when (t_value <: t) && (t <: t_var) -> e                                         (* (Assign4) *)
	| Type t_var, _, _                                     -> error 9 "not satisfy rule (Assign2) & (Assign4)"

(**
 * TODO
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
				if ref @< r then
					let re = if cut then (r @-- ref) else r in
					(re, t) :: e
				else e
		) [] env

(**
 * TODO
 * @param e  type environment
 * @param ns namespace where the reference will be resolved
 * @param r  reference to be resolved
 *)
let rec resolve e ns r =
	match r with
	| "ROOT"   :: rs -> ([], type_of e !!rs)
	| "PARENT" :: rs -> if ns = [] then error 4 "PARENT of root namespace is impossible"
	                    else (prefix ns, type_of e !!((prefix ns) @++ rs))
	| "THIS"   :: rs -> (ns, type_of e !!(ns @++ rs))
	| _              ->
		if ns = [] then ([], type_of e r)
		else
			let t = type_of e (ns @<< r) in
			if t = Undefined then resolve e (prefix ns) r
			else (ns, t)

(**
 * TODO
 * @param e     type environment
 * @param proto prototype reference whose attributes to be copied
 * @param dest  reference of destination
 *)
let copy e proto dest =
	let e_proto = env_of_ref e proto true in
	List.fold_left (fun ep (rep, tep) -> (dest @++ rep, tep) :: ep) e e_proto

(**
 * TODO
 * @param e     type environment
 * @param ns    namespace where references will be resolved
 * @param proto reference of prototype
 * @param r     target variable
 *)
let inherit_env e ns proto r =
	let get_proto =
		match resolve e ns proto with
		| _, Undefined                    -> error 5 ("prototype is not found: " ^ !^proto)
		| nsx, Type TBasic TObject
		| nsx, Type TBasic TSchema (_, _) -> nsx @++ proto
		| _, Type t                       -> error 6 ("invalid prototype " ^ !^r ^ ":" ^ (string_of_type t))
	in
	copy e get_proto r

(*******************************************************************
 * second-pass type environment
 *******************************************************************)

(* TODO resolve forward type assignment for lazy & data reference *)
let rec resolve_tforward e ns r acc =
	let follow_tforward nsp tr =
		let rp = nsp @++ r in
		if rp @<= tr then error 10 ("implicit cyclic reference " ^ !^tr)
		else resolve_tforward e rp tr (SetRef.add rp acc)
	in
	if SetRef.exists (fun rx -> rx = r) acc then error 11 ("cyclic reference " ^ !^r)
	else
		match resolve e ns r with
		| _, Undefined                    -> error 12 ("undefined reference " ^ !^r ^ " in " ^ !^ns)
		| nsp, Type TForward (tr, islink) -> follow_tforward nsp tr
		| nsp, Type t                     -> (nsp @++ r, t)

(* TODO replace all TForward elements in environment 'e' *)
let replace_tforward_in_env e =
	let main = ["main"] in
	let replace_tforward e1 r t tr islink =
		let (proto, t_val) =
			match resolve_tforward e1 r tr SetRef.empty with
			| proto, TBasic t -> (proto, (if islink then TBasic t else TRef t))
			| proto, t        -> (proto, t)
		in
		let e2 = (r, t_val) :: e1 in
		if t_val <: TBasic TObject then copy e2 proto r
		else e2
	in
	let rec iter e3 src =
		match src with
		| [] -> e3
		| (r, t) :: tail ->
			if not (main @<= r) || r = main then iter e3 tail
			else
				let result =
					match t with
					| TForward (tr, islink) -> replace_tforward e3 r t tr islink
					| _  -> (r, t) :: e3
				in
				iter result tail
	in
	iter e e

(* perform second valuation of environment 'e' *)
let second_pass_eval e = replace_tforward_in_env e

let get_main e =
	let main = ["main"] in
	let rec iter e buf =
		match e with
		| [] -> buf
		| (r, t) :: tail ->
			if not (main @<= r) then iter tail buf
			else
				let r1 = List.tl r in
				if r1 = [] || domain buf r1 then iter tail buf else iter tail ((r1, t) :: buf)
	in
	iter e []

(*******************************************************************
 * type inference functions
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
		| _, Type (TVec _)   -> error 101 ("dereference of " ^ !^r ^ " is a vector")
		| _, Type TUndefined -> error 102 ("dereference of " ^ !^r ^ " is TUndefined")
		| _, Undefined       -> TForward (r, false)

let sfLinkReference lr =  (* (Deref Link) *)
	(**
	 * @param ns namespace
	 * @param e  type environment
	 *)
	fun e ns r ->
		let link = sfReference lr in
		match resolve e ns link with
		| nsp, Undefined -> (nsp @++ link, TForward (link, true))
		| nsp, Type t    -> (nsp @++ link, t)

let rec sfVector vec =
	(**
	 * @param ns namespace
	 * @param e  type environment
	 *)
	fun e ns ->
		let rec eval v =  (* (Vec) *)
			match v with
			| [] -> TUndefined
			| head :: tail ->
				let t_head =
					match sfBasicValue head e ns with
					| TRef _ | TForward _ -> TRef TNull
					| t                   -> t
				in
				if tail = [] then t_head
				else if t_head = eval tail then t_head
				else error 105 "types of vector elements are different"
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
			if first then assign e r t_val (TBasic TObject)          (* (Proto1) *)
			else e
		| B_P (pb, p)    ->
			let t_block =
				if first && t_val = TUndefined then TBasic TObject   (* (Proto2) *)
				else t_val
			in
			let e1 = assign e r t_val t_block in
			sfPrototype p false t_block ns r (sfpBlock pb r e1)
		| R_P (pr, p)    ->
			let proto = sfReference pr in
			match resolve e ns proto with
			| _, Undefined -> error 106 ("prototype is not found: " ^ !^proto)
			| _, Type t    ->
				let e_proto = assign e r t_val t in                                (* (Proto3) & (Proto4) *)
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
		| BV bv          -> assign e r t (sfBasicValue bv e ns)
		| LR link        ->
			let (r_link, t_link) = sfLinkReference link e ns r in
			let e1 = assign e r t t_link in
			if t_link <: TBasic TObject then copy e1 r_link r
			else e1
		| P (schema, proto) ->
			match schema with
			| SID sid ->
				(
					match type_of e [sid] with
					| Undefined  -> error 107 ("schema " ^ sid ^ " is not exist")
					| Type t_sid ->
						if not (is_schema t_sid) then error 107 (sid ^ " is not a schema")
						else
							let e1 = inherit_env e ns [sid] r in
							sfPrototype proto true t_sid ns r e1
				)
			| EmptySchema -> sfPrototype proto true TUndefined ns r e

and sfAssignment (r, t, v) =
	(**
	 * @param ns namespace
	 * @param e  type environment
	 *)
	fun ns e -> sfValue v ns (ns @++ r) t e

and sfpBlock block =
	(**
	 * @param ns namespace
	 * @param e  type environment
	 *)
	fun ns e ->
		match block with
		| A_B (a, b) -> sfpBlock b ns (sfAssignment a ns e)
		| EmptyBlock -> e

and sfpSchema s =
	let (sid, parent, b) = s in
	fun e ->
		let r_sid = [sid] in
		if domain e r_sid then error 107 ("cannot redefined schema " ^ sid);
		let define_schema r_superid supertype =
			let t_sid = (TBasic (TSchema (sid, supertype))) in
			let e1 = assign e r_sid TUndefined t_sid in
			let e2 =
				if r_superid != [] then inherit_env e1 [] r_superid r_sid
				else e1
			in
			sfpBlock b r_sid e2
		in
		match parent with
		| EmptySchema -> define_schema [] TRootSchema
		| SID superid ->
			match type_of e [superid] with
			| Undefined        -> error 108 ("super schema " ^ superid ^ " is not exist")
			| Type (TBasic t)  ->
				if is_schema (TBasic t) then define_schema [superid] t
				else error 108 (superid ^ " is not a schema")
			| _                -> error 110 (superid ^ " is not a schema")

and sfpContext ctx =
	fun e ->
		match ctx with
		| A_C (a, c) -> sfpContext c (sfAssignment a [] e)
		| S_C (s, c) -> sfpContext c (sfpSchema s e)
		| EmptyContext -> e

and sfpSpecification sfp =
	let e1 = sfpContext sfp [] in
	if not (domain e1 ["main"]) then error 200 "main object is not exist"
	else get_main (second_pass_eval e1)

