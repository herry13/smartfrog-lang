open Common
open Sfsyntax

(*******************************************************************
 * type environment
 *******************************************************************)

type env = _type MapRef.t

type _t = Type of _type
        | Undefined
and var = string list * _type
and lenv = var list

(*******************************************************************
 * helper functions
 *******************************************************************)

let rec (@==) r1 r2 =
	match r1, r2 with
	| [], [] -> true
	| id1 :: rs1, id2 :: rs2 when id1 = id2 -> rs1 @== rs2
	| _ -> false

(* alias of functions from module Sfdomain *)
let (@++)  = Sfdomain.(@++);;
let (@--)  = Sfdomain.(@--);;
let prefix = Sfdomain.prefix;;
let (@<=)  = Sfdomain.(@<=);;
let (@<)   = Sfdomain.(@<);;
let (!!)   = Sfdomain.(!!);;
let (@<<)  = Sfdomain.(@<<);;
let (!^)   = Sfdomain.(!^);;

exception TypeError of int * string

let error (code: int) (msg: string) : 'a = raise (TypeError (code, "[type-err" ^ string_of_int(code) ^ "] " ^ msg))

let string_of_env (e: env) =
	MapRef.fold (fun r t s -> s ^ (!^r) ^ " : " ^ (string_of_type t) ^ "\n") e ""

(** convert a type environment to a map **)
let map_of (le: lenv) : env =
	List.fold_left (fun acc (r, t) -> MapRef.add r t acc) MapRef.empty le

let type_of (r: reference) (e: env) : _type = MapRef.find r e

(*******************************************************************
 * typing judgement functions
 *******************************************************************)

(** return the type of variable 'r' **)
let rec find (e: lenv) (r: reference) : _t =
	match e with
	| []               -> Undefined
	| (vr, vt) :: tail -> if r = vr then Type vt else find tail r

(** return true if variable 'r' is in environment 'e', otherwise false *)
let rec domain (e: lenv) (r: reference) : bool =
	match e with
	| []               -> false
	| (vr, vt) :: tail -> if r = vr then true else (domain tail r)

(* return true if type 't1' is a sub-type of 't2', otherwise false *)
let rec (<:) (t1: _type) (t2: _type) : bool =                             (* Subtyping rules  *)
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
let rec has_type (e: lenv) (t: _type) : bool =
	match t with
	| TUndefined                -> false
	| TForward (_, _)           -> true                    (* TODO (Type Forward) *)
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

let well_typed (e: lenv) : bool =
	let rec iter e env: bool =
		match e with
		| []             -> true
		| (r, t) :: tail ->
			if (has_type env t) then iter tail env
			else false
	in
	iter e e

let is_schema (t: _type) : bool = t <: (TBasic TRootSchema)

let rec object_of_schema (t: basicType) : basicType =
	match t with
	| TSchema (sid, TRootSchema) -> TSchema (sid, TObject)
	| TSchema (sid, super) -> TSchema (sid, object_of_schema super)
	| _  -> error 401 "cannot create type object of a non-schema type"

(*******************************************************************
 * type assignment functions
 *******************************************************************)

(* bind type 't' to variable 'r' into environment 'e' *)
let bind (e: lenv) (r: reference) (t: _type) : lenv =
	if (find e r) != Undefined then error 1 ("cannot bind type to an existing variable " ^ !^r)
	else if (List.length r) > 1 then
		match find e (prefix r) with
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
let assign (e: lenv) (r: reference) (t: _type) (t_value: _type) : lenv =
	match (find e r), t, t_value with
	| Undefined, TUndefined, _                             -> bind e r t_value                          (* (Assign1) *)
	| Undefined, t, _ when t_value <: t                    -> bind e r t                                (* (Assign3) *)
	| Undefined, t, TForward (r, islink)                   -> (r, t) :: (r, TForward (r, islink)) :: e  (* TODO (Assign5) *)
	| Undefined, _, _                                      -> error 7 (!^r ^ " not satisfy rule (Assign3)")
	| Type (TForward (r, islink)), _, _                    -> (r, t) :: (r, t_value) :: e
	| Type t_var, TUndefined, _ when t_value <: t_var      -> e                                         (* (Assign2) *)
	| Type t_var, TUndefined, TForward (r, islink)         -> (r, TForward (r, islink)) :: e            (* TODO (Assign6) *)
	| Type t_var, TUndefined, _                            -> error 8 (!^r ^ " not satisfy rule (Assign2)")
	| Type t_var, _, _ when (t_value <: t) && (t <: t_var) -> e                                         (* (Assign4) *)
	| Type t_var, _, _                                     -> error 9 (!^r ^ " not satisfy rule (Assign2) & (Assign4)")

(**
 * TODO
 * return part of environment 'env' where 'ref' is the prefix of the variables
 * the prefix of variables will be removed when 'cut' is true, otherwise
 * the variables will have original references
 *)
let rec env_of_ref (env: lenv) (ref: reference) (cut: bool) : lenv =
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
let rec resolve (e: lenv) (ns: reference) (r: reference) : (reference * _t) =
	match ns, r with
	| _, "ROOT"   :: rs -> ([], find e !!rs)
	| _, "PARENT" :: rs -> if ns = [] then error 4 "PARENT of root namespace is impossible"
	                       else (prefix ns, find e !!((prefix ns) @++ rs))
	| _, "THIS"   :: rs -> (ns, find e !!(ns @++ rs))
	| [], _             -> ([], find e r)
	| _, _              ->
		match find e (ns @<< r) with
		| Undefined -> resolve e (prefix ns) r
		| t         -> (ns, t)

(**
 * TODO
 * @param e     type environment
 * @param proto prototype reference whose attributes to be copied
 * @param dest  reference of destination
 *)
let copy (e: lenv) (proto: reference) (dest: reference) : lenv =
	let e_proto = env_of_ref e proto true in
	List.fold_left (fun ep (rep, tep) -> (dest @++ rep, tep) :: ep) e e_proto

(**
 * TODO
 * @param e     type environment
 * @param ns    namespace where references will be resolved
 * @param proto reference of prototype
 * @param r     target variable
 *)
let inherit_env (e: lenv) (ns: reference) (proto: reference) (r: reference) : lenv =
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
let rec resolve_tforward (e: lenv) (ns: reference) (r: reference) (acc: SetRef.t) : (reference * _type) =
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
let replace_tforward_in_env (e: lenv) : lenv =
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
let second_pass_eval (e: lenv) : lenv =
	let e1 = replace_tforward_in_env e in
	if well_typed e1 then e1
	else error 201 "not well-typed"

let get_main (e: lenv) : lenv =
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

let sfDataReference dr : lenv -> reference -> _type =  (* (Deref Data) *)
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

let sfLinkReference lr : lenv -> reference -> reference -> (reference * _type) =  (* (Deref Link) *)
	(**
	 * @param ns namespace
	 * @param e  type environment
	 *)
	fun e ns r ->
		let link = sfReference lr in
		match resolve e ns link with
		| nsp, Undefined -> (nsp @++ link, TForward (link, true))
		| nsp, Type t    -> (nsp @++ link, t)

let rec sfVector vec : lenv -> reference -> _type =
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

and sfBasicValue bv : lenv -> reference -> _type =
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
let rec sfPrototype proto first t_val : reference -> reference -> lenv -> lenv =
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

and sfValue v : reference -> reference -> _type -> lenv -> lenv =
	(**
	 * @param ns namespace
     * @param r  variable's reference
	 * @param t  predefined type
	 * @param e  type environment
	 *)
	fun ns r t e ->
		match v with
		| BV bv   -> assign e r t (sfBasicValue bv e ns)
		| LR link ->
			(
				let (r_link, t_link) = sfLinkReference link e ns r in
				let e1 = assign e r t t_link in
				if t_link <: TBasic TObject then copy e1 r_link r
				else e1
			)
		| Ac a              -> assign e r t (TBasic TAction)
		| P (schema, proto) ->
			match schema with
			| SID sid ->
				(
					match find e [sid] with
					| Undefined  -> error 107 ("schema " ^ sid ^ " is not exist")
					| Type TBasic TSchema (sid, super) when (TBasic super) <: (TBasic TRootSchema) ->
						let t_sid = TBasic (object_of_schema (TSchema (sid, super))) in
						let e1 = inherit_env e ns [sid] r in
						sfPrototype proto true t_sid ns r e1
					| _ -> error 107 (sid ^ " is not a schema")
				)
			| EmptySchema -> sfPrototype proto true TUndefined ns r e

and sfAssignment (r, t, v) : reference -> lenv -> lenv =
	(**
	 * @param ns namespace
	 * @param e  type environment
	 *)
	fun ns e -> sfValue v ns (ns @++ r) t e

and sfpBlock block : reference -> lenv -> lenv =
	(**
	 * @param ns namespace
	 * @param e  type environment
	 *)
	fun ns e ->
		match block with
		| A_B (a, b) -> sfpBlock b ns (sfAssignment a ns e)
		| G_B (g, b) -> sfpBlock b ns (sfpGlobal g e)
		| EmptyBlock -> e

and sfpSchema s : lenv -> lenv =
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
			match find e [superid] with
			| Undefined        -> error 108 ("super schema " ^ superid ^ " is not exist")
			| Type (TBasic t)  ->
				if is_schema (TBasic t) then define_schema [superid] t
				else error 108 (superid ^ " is not a schema")
			| _                -> error 110 (superid ^ " is not a schema")

and sfpGlobal g : lenv -> lenv =
	fun e -> assign e ["global"] TUndefined (TBasic TGlobal)

and sfpContext ctx : lenv -> lenv =
	fun e ->
		match ctx with
		| A_C (a, c) -> sfpContext c (sfAssignment a [] e)
		| S_C (s, c) -> sfpContext c (sfpSchema s e)
		| G_C (g, c) -> sfpContext c (sfpGlobal g e)
		| EmptyContext -> e

and sfpSpecification sfp : env =
	let e1 = sfpContext sfp [] in
	if not (domain e1 ["main"]) then error 200 "main object is not exist"
	else
		let e2 = get_main (second_pass_eval e1) in
		let e_main = assign e2 ["global"] TUndefined (TBasic TGlobal) in
		map_of e_main


(*******************************************************************
 * a map from type to set of values
 *******************************************************************)

module MapType = Map.Make ( struct
	type t = _type
	let compare = Pervasives.compare
end )

type typevalue = Sfdomain.SetValue.t MapType.t

let values_of (t: _type) (map: typevalue) : Sfdomain.SetValue.t =
	if MapType.mem t map then MapType.find t map
	else Sfdomain.SetValue.empty

let add_value (t: _type) (v: Sfdomain.value) (map: typevalue) : typevalue =
	MapType.add t (Sfdomain.SetValue.add v (values_of t map)) map

let make_typevalue (env_0: env) (fs_0: Sfdomain.flatstore) (env_g: env) (fs_g: Sfdomain.flatstore) : typevalue =
	(** group action effects' values based on their type **)
	let add_action_values (e: env) map =
		let actions = values_of (TBasic TAction) map in
		let add_effect_values =
			List.fold_left (
				fun acc (r, v) ->
					match v with
					| Sfdomain.Boolean _ -> add_value (TBasic TBool) (Sfdomain.Basic v) acc
					| Sfdomain.Number  _ -> add_value (TBasic TNum) (Sfdomain.Basic v) acc
					| Sfdomain.String  _ -> add_value (TBasic TStr) (Sfdomain.Basic v) acc
					| Sfdomain.Vector  _ -> error 501 "adding vector value of effects" (* TODO *)
					| _         -> acc
			)
		in
		Sfdomain.SetValue.fold (
			fun v map ->
				match v with
				| Sfdomain.Action (n, ps, c, pre, eff) -> add_effect_values map eff
				| _                           -> map
		) actions map
	in
	let null = Sfdomain.Basic Sfdomain.Null in
	let rec add_object (t: basicType) (v: Sfdomain.value) (t_next: basicType) map =
		let map1 = add_value (TBasic t) v map in
		let map2 = add_value (TRef t) v map1 in
		let map3 = add_value (TRef t) null map2 in
		match t_next with
		| TObject              -> add_value (TBasic TObject) v map3
		| TSchema (sid, super) -> add_object t_next v super map3
		| _                    -> error 502 "super-type is not a schema or an object type"
	in
	let add_store_values (e: env) =
		MapRef.fold (
			fun r v (map: typevalue) ->
				match type_of r e with
				| TUndefined -> error 503 ("Type of " ^ !^r ^ " is undefined.")
				| TBasic TSchema (sid, super) ->
					add_object (TSchema (sid, super)) (Sfdomain.Basic (Sfdomain.Ref r)) super map
				| t -> add_value t v map
		)
	in
	let table00 = add_store_values env_0 fs_0 MapType.empty in
	let table01 = add_action_values env_0 table00 in
	let table10 = add_store_values env_g fs_g table01 in
	add_action_values env_g table10
