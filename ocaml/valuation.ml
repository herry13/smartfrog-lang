open Syntax

let sfBoolean b = if b = "true" then Domain.Boolean true else Domain.Boolean false

let int_regex = Str.regexp "\\(^-?[0-9]+$\\)"

let sfNumber n =
	if Str.string_match int_regex n 0 then Domain.Number (Domain.Int (int_of_string n))
	else Domain.Number (Domain.Float (float_of_string n))

let sfString s = Domain.String s

let sfNull = Domain.Null

let sfReference r = r

let sfDataReference dr = Domain.Ref (sfReference dr)

let sfLinkReference lr =
	let rp = sfReference lr in
	fun r -> if Domain.(@<=) rp r then Domain.error 4 else Domain.Link rp

let rec sfVector vec =
	let rec eval v =
		match v with
		| [] -> []
		| head :: tail -> (sfBasicValue head) :: (eval tail)
	in
	Domain.Vector (eval vec)

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
		| R_P (pr, p) -> sfPrototype p ns r (Domain.inherit_proto s ns (sfReference pr) r)
		| EmptyPrototype    -> s

and sfValue v =
	fun ns r s ->
		match v with
		| BV bv              -> Domain.bind s r (Domain.Basic (sfBasicValue bv))
		| LR lr              -> Domain.bind s r (sfLinkReference lr r)
		| P (EmptySchema, p) -> sfPrototype p ns r (Domain.bind s r (Domain.Store []))
		| P (SID sid, p)     ->
			let s1 = Domain.bind s r (Domain.Store []) in
			let s2 = Domain.inherit_proto s1 [] [sid] r in
			sfPrototype p ns r s2
		| Ac a               -> sfpAction a ns r s

(** 't' (type) is ignored since this function only evaluates the value **)
and sfAssignment (r, t, v) =
	fun ns s -> sfValue v ns (Domain.(@++) ns r) s

and sfBlock block =
	fun ns s ->
		match block with
		| A_B (a, b) -> sfBlock b ns (sfAssignment a ns s)
		| G_B (g, b) -> sfBlock b ns (sfpGlobal g s)
		| EmptyBlock -> s

and sfpSchema (sid, parent, b) =
	fun s ->
		let r_sid = [sid] in
		let s1 = Domain.bind s r_sid (Domain.Store []) in
		let s2 =
			match parent with
			| EmptySchema -> s1
			| SID superid -> Domain.inherit_proto s1 [] [superid] r_sid
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
	let v1 = Domain.find s1 r in
	let s2 =
		match v1 with
		| Domain.Val (Domain.Store s) -> Domain.accept s1 r s r
		| _ -> Domain.error 11
	in
	let v2 = Domain.find s2 r in
	let rg = ["global"] in
	let add_global s =
		match Domain.find s1 rg with
		| Domain.Undefined -> s
		| Domain.Val (Domain.Global vg) -> Domain.bind s rg (Domain.Global vg)
		| _ -> Domain.error 12
	in
	match v2 with
	| Domain.Val (Domain.Store s) -> add_global s
	| _ -> Domain.error 11


(** global constraints **)
and sfpGlobal g =
	fun s ->
		let r = ["global"] in
		let gc = sfpConstraint g in
		match Domain.find s r with
		| Domain.Val (Domain.Global gs) ->
			let f = Domain.Global (Domain.And [gc; gs]) in
			Domain.bind s r f
		| Domain.Undefined -> Domain.bind s r (Domain.Global gc)
		| _                  -> Domain.error 12

(** constraints **)
and sfpEqual r bv = Domain.Eq (sfReference r, sfBasicValue bv)

and sfpNotEqual r bv = Domain.Ne (sfReference r, sfBasicValue bv)

and sfpNegation c = Domain.Not (sfpConstraint c)

and sfpImplication c1 c2 = Domain.Imply (sfpConstraint c1, sfpConstraint c2)

and sfpMembership r vec =
	let rec eval v =
		match v with
		| [] -> []
		| head :: tail -> (sfBasicValue head) :: (eval tail)
	in
	Domain.In (sfReference r, eval vec)

and sfpConjunction cs = Domain.And (List.fold_left (fun acc c -> (sfpConstraint c) :: acc) [] cs)

and sfpDisjunction cs = Domain.Or (List.fold_left (fun acc c -> (sfpConstraint c) :: acc) [] cs)

and sfpConstraint (c : _constraint) =
	match c with
	| Eq (r, v)      -> sfpEqual r v
	| Ne (r, v)      -> sfpNotEqual r v
	| Not _          -> sfpNegation c
	| Imply (c1, c2) -> sfpImplication c1 c2
	| In (r, vec)    -> sfpMembership r vec
	| And cs         -> sfpConjunction cs
	| Or cs          -> sfpDisjunction cs

(* action *)
and sfpAction (params, _cost, conds, effs) =
	let parameters =
		List.fold_left (fun acc (id, t) ->
			match t with
			| TBasic TNum | TBasic TStr -> Domain.error 101
			| _ -> (id, t) :: acc
		) [] params
	in
	let cost =
		match _cost with
		| Cost cs   -> int_of_string cs
		| EmptyCost -> 1
	in
	let conditions =
		match conds with
		| EmptyCondition -> Domain.True
		| Cond c         -> sfpConstraint c
	in
	let effects = 
		List.fold_left (fun acc (r, bv) -> (r, sfBasicValue bv) :: acc) [] effs
	in
	fun ns r s ->
		let a = (r, parameters, cost, conditions, effects) in
		Domain.bind s r (Domain.Action a)
