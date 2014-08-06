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
	| Null    -> sfNull
	| Vector vec -> sfVector vec
	| DR dr   -> sfDataReference dr

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
		| LR lr      -> Sfdomain.bind s r (Sfdomain.Basic (sfLinkReference lr r))
		| P (sid, p) -> sfPrototype p ns r (Sfdomain.bind s r (Sfdomain.Store []))

(** 't' (type) is ignored since this function only evaluates the value **)
and sfAssignment (r, t, v) =
	fun ns s -> sfValue v ns (Sfdomain.ref_plus_ref ns r) s

and sfBlock block =
	fun ns s ->
		match block with
		| A_B (a, b) -> sfBlock b ns (sfAssignment a ns s)
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
		| A_C (a, c) -> sfpContext c (sfAssignment a [] s)
		| S_C (sc, c) -> sfpContext c (sfpSchema sc s)
		| EmptyContext -> s

and sfpSpecification sfp =
	let r = ["main"] in
	let s1 = sfpContext sfp [] in
	let v1 = Sfdomain.find s1 r in
	let s2 =
		match v1 with
		| Sfdomain.Val (Sfdomain.Store s) -> Sfdomain.accept s1 r s r
		| _ -> Sfdomain.error 9
	in
	let v2 = Sfdomain.find s2 r in
	match v2 with
	| Sfdomain.Val (Sfdomain.Store s) -> s
	| _ -> Sfdomain.error 9

