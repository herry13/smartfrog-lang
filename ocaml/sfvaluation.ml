open Sfsyntax

let sfBoolean b = if b = "true" then Sfdomain.Boolean true else Sfdomain.Boolean false

let int_regex = Str.regexp "\\(^-?[0-9]+$\\)"

let sfNumber n =
	if Str.string_match int_regex n 0 then Sfdomain.Number (Sfdomain.Int (int_of_string n))
	else Sfdomain.Number (Sfdomain.Float (float_of_string n))

let sfString s = Sfdomain.String s

let sfNull = Sfdomain.Null

let sfReference r = r

let sfDataReference dr = Sfdomain.Ref (sfReference dr)

let sfLinkReference lr =
	let rp = sfReference lr in
	fun r -> if Sfdomain.ref_prefixeq_ref rp r then Sfdomain.error 4 else Sfdomain.Link rp

let rec sfVector vec =
	let rec eval v =
		match v with
		| [] -> []
		| head :: tail -> (sfBasicValue head) :: (eval tail)
	in
	Sfdomain.Vector (eval vec)

and sfBasicValue bv =
	match bv with
	| Boolean b  -> sfBoolean b
	| Number n   -> sfNumber n
	| String s   -> sfString s
	| Null       -> sfNull
	| Vector vec -> sfVector vec
	| DR dr      -> sfDataReference dr

let rec sfPrototype ps =
	fun ns r s ->
		match ps with
		| B_P (pb, p) -> sfPrototype p ns r (sfBlock pb r s)
		| R_P (pr, p) -> sfPrototype p ns r (Sfdomain.inherit_proto s ns (sfReference pr) r)
		| EmptyPrototype    -> s

and sfValue v =
	fun ns r s ->
		match v with
		| BV bv      -> Sfdomain.bind s r (Sfdomain.Basic (sfBasicValue bv))
		| LR lr      -> Sfdomain.bind s r (sfLinkReference lr r)
		| P (sid, p) -> sfPrototype p ns r (Sfdomain.bind s r (Sfdomain.Store []))
		| Ac a       -> sfpAction a ns r s

(** 't' (type) is ignored since this function only evaluates the value **)
and sfAssignment (r, t, v) =
	fun ns s -> sfValue v ns (Sfdomain.ref_plus_ref ns r) s

and sfBlock block =
	fun ns s ->
		match block with
		| A_B (a, b) -> sfBlock b ns (sfAssignment a ns s)
		| G_B (g, b) -> sfBlock b ns (sfpGlobal g s)
		| EmptyBlock   -> s

and sfpSchema (sid, parent, b) =
	fun s ->
		let r_sid = [sid] in
		let s1 = Sfdomain.bind s r_sid (Sfdomain.Store []) in
		let s2 =
			match parent with
			| EmptySchema -> s1
			| SID superid -> Sfdomain.inherit_proto s1 [] [superid] r_sid
		in
		sfBlock b r_sid s2

and sfpContext ctx =
	fun s ->
		match ctx with
		| A_C (a, c)   -> sfpContext c (sfAssignment a [] s)
		| S_C (sc, c)  -> sfpContext c (sfpSchema sc s)
		| G_C (g, c)   -> sfpContext c (sfpGlobal g s)
		| EmptyContext -> s

and sfpSpecification sfp =
	let r = ["main"] in
	let s1 = sfpContext sfp [] in
	let v1 = Sfdomain.find s1 r in
	let s2 =
		match v1 with
		| Sfdomain.Val (Sfdomain.Store s) -> Sfdomain.accept s1 r s r
		| _ -> Sfdomain.error 11
	in
	let v2 = Sfdomain.find s2 r in
	let rg = ["global"] in
	let add_global s =
		match Sfdomain.find s1 rg with
		| Sfdomain.Undefined -> s
		| Sfdomain.Val (Sfdomain.Global vg) -> Sfdomain.bind s rg (Sfdomain.Global vg)
		| _ -> Sfdomain.error 12
	in
	match v2 with
	| Sfdomain.Val (Sfdomain.Store s) -> add_global s
	| _ -> Sfdomain.error 11


(** global constraints **)
and sfpGlobal g =
	fun s ->
		let r = ["global"] in
		let gc = sfpConstraint g in
		match Sfdomain.find s r with
		| Sfdomain.Val (Sfdomain.Global gs) ->
			let f = Sfdomain.Global (Sfdomain.And [gc; gs]) in
			Sfdomain.bind s r f
		| Sfdomain.Undefined               -> Sfdomain.bind s r (Sfdomain.Global gc)
		| _                                -> Sfdomain.error 12

(** constraints **)
and sfpEqual (r, bv) = Sfdomain.Eq (sfReference r, sfBasicValue bv)

and sfpNotEqual (r, bv) = Sfdomain.Ne (sfReference r, sfBasicValue bv)

and sfpNegation c = Sfdomain.Not (sfpConstraint c)

and sfpImplication (c1, c2) = Sfdomain.Imply (sfpConstraint c1, sfpConstraint c2)

and sfpMembership (r, vec) =
	let rec eval v =
		match v with
		| [] -> []
		| head :: tail -> (sfBasicValue head) :: (eval tail)
	in
	Sfdomain.In (sfReference r, eval vec)

and sfpConjunction cs = Sfdomain.And (List.fold_left (fun acc c -> (sfpConstraint c) :: acc) [] cs)

and sfpDisjunction cs = Sfdomain.Or (List.fold_left (fun acc c -> (sfpConstraint c) :: acc) [] cs)

and sfpConstraint (c : _constraint) =
	match c with
	| Eq e -> sfpEqual e
	| Ne e -> sfpNotEqual e
	| Not e -> sfpNegation e
	| Imply e -> sfpImplication e
	| In e -> sfpMembership e
	| And e -> sfpConjunction e
	| Or e -> sfpDisjunction e

(* action *)
and sfpAction (params, _cost, conds, effs) =
	let parameters =
		List.fold_left (fun acc (id, t) -> (id, t) :: acc) [] params
	in
	let cost =
		match _cost with
		| Cost cs   -> int_of_string cs
		| EmptyCost -> 1
	in
	let conditions =
		match conds with
		| EmptyCondition -> Sfdomain.True
		| Cond c         -> sfpConstraint c
	in
	let effects = 
		List.fold_left (fun acc (r, bv) -> (r, sfBasicValue bv) :: acc) [] effs
	in
	fun ns r s ->
		let a = (parameters, cost, conditions, effects) in
		Sfdomain.bind s r (Sfdomain.Action a)
