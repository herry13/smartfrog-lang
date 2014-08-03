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


(*******************************************************************
 * type checking rules
 *******************************************************************)

let rec type_of e r =
	match e with
	| []               -> Undefined
	| (vr, vt) :: tail -> if r == vr then Type vt else type_of tail r

let rec dom e r =
	match e with
	| []               -> false
	| (vr, vt) :: tail -> if r == vr then true else (dom e r)

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

let bind e r t =
	if (type_of e r) != Undefined then failure 1 ("cannot bind an existing variable " ^ (string_of_ref r))
	else (r, t) :: e

let rec has_prefix e prefix =
	if prefix = [] then e
	else if e = [] then []
	else List.fold_left (fun ex (r, t) -> print_string ((string_of_ref prefix) ^ " < " ^ (string_of_ref r) ^ " = " ^ (string_of_bool (prefix < r)) ^ "\n"); if prefix < r then (r, t) :: ex else ex) [] e

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

let inherit_env e ns proto r =
	print_string ("inherit " ^ (string_of_ref proto) ^ "\n");
	let ex = has_prefix e proto in
	print_string ((string_of_env ex) ^ "\n\n");
	e (* TODO *)

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
	fun ns e -> sfValue v ns (Sfdomain.(++) ns r) e

and sfBlock block =
	fun ns e ->
		match block with
		| A_B (a, b) -> sfBlock b ns (sfAssignment a ns e)
		| EmptyBlock -> e

and sfSpecification sf = sfBlock sf [] []


(**************
 * test
 **************)

(* let test =
	let e1 = bind [] ["a"] (Tbasic Tobj) in
	let e2 = bind e1 ["a"] (Tbasic Tnum) in
	print_string (string_of_env e2);
	print_string "\n" *)
