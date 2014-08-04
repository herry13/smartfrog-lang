open Sfsyntax

let sfBoolean b = if b = "true" then Sfdomain.Boolean true else Sfdomain.Boolean false

let sfNumber n =
	let f = float_of_string n in
	let i = int_of_float f in
	if f = (float_of_int i) then Sfdomain.Number (Sfdomain.Int i) else Sfdomain.Number (Sfdomain.Float f)

let sfString s = Sfdomain.String s

let sfNull = Sfdomain.Null

let sfReference r = r

let sfDataReference dr = Sfdomain.Ref (sfReference dr)

let sfLinkReference lr =
	let rp = sfReference lr in
	fun r -> if Sfdomain.ref_prefixeq_ref rp r then Sfdomain.failure 4 else Sfdomain.Link rp

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
		| BV bv -> Sfdomain.bind s r (Sfdomain.Basic (sfBasicValue bv))
		| LR lr -> Sfdomain.bind s r (Sfdomain.Basic (sfLinkReference lr r))
		| P p   -> sfPrototype p ns r (Sfdomain.bind s r (Sfdomain.Store []))

(** 't' (type) is ignored since this function only evaluates the value **)
and sfAssignment (r, t, v) =
	fun ns s -> sfValue v ns (Sfdomain.ref_plus_ref ns r) s

and sfBlock block =
	fun ns s ->
		match block with
		| A_B (a, b) -> sfBlock b ns (sfAssignment a ns s)
		| EmptyBlock   -> s

and sfSpecification b =
	let r = ["sfConfig"] in
	let s1 = sfBlock b [] [] in
	let v1 = Sfdomain.find s1 r in
	let s2 =
		match v1 with
		| Sfdomain.Val (Sfdomain.Store s) -> Sfdomain.accept s1 r s r
		| _ -> Sfdomain.failure 9
	in
	let v2 = Sfdomain.find s2 r in
	match v2 with
	| Sfdomain.Val (Sfdomain.Store s) -> s
	| _ -> Sfdomain.failure 9
