(*******************************************************************
 * semantics domain
 *******************************************************************)
(** core elements **)
type number   = Int of int
              | Float of float
and vector    = basic list
and basic     = Boolean of bool
              | Number of number
              | String of string
              | Null
              | Vector of vector
              | Ref of reference
and value     = Basic of basic
              | Store of store
              | Global of _constraint
              | Link of reference
              | Action of action
and _value    = Val of value
              | Undefined
and cell      = ident * value
and store     = cell list
and reference = ident list
and ident     = string

(** constraint elements **)
and _constraint = Eq of reference * basic
                | Ne of reference * basic
                | Not of _constraint
                | Imply of _constraint * _constraint
                | And of _constraint list
                | Or of _constraint list
                | In of reference * vector
                | True
                | False

(** action elements **)
and action     = reference * parameters * cost * conditions * effects
and parameters = param list
and param      = ident * Sfsyntax._type
and cost       = int
and conditions = _constraint
and effects    = effect list
and effect     = reference * basic

(*******************************************************************
 * helpers
 *******************************************************************)

(** exception for any error on semantics algebra **)
exception SfError of int * string

(**
 * receive and print semantics error message
 * @code int error code
 *)
let error code = raise (SfError (code, "[err" ^ (string_of_int code) ^ "]"))

(* Module set of reference *)
module SetRef = Set.Make
	(
		struct
			let compare = Pervasives.compare
			type t = reference
		end
	)

let (!^) r = String.concat "." r

(*******************************************************************
 * semantics algebras
 *******************************************************************)
let rec prefix r =
	match r with
	| [] -> []
	| head::tail -> if tail = [] then [] else head :: (prefix tail)

let (!-) r = prefix r

let ref_plus_ref r1 r2 = List.concat [r1; r2]

let (@++) r1 r2 = ref_plus_ref r1 r2

let ref_plus_id r id = ref_plus_ref r [id]

let (@+.) r id = ref_plus_id r id

let rec ref_minus_ref r1 r2 =
	if r1 = [] then []
	else if r2 = [] then r1
	else if (List.hd r1) = (List.hd r2) then ref_minus_ref (List.tl r1) (List.tl r2)
	else r1

let (@--) r1 r2 = ref_minus_ref r1 r2

let ref_prefixeq_ref r1 r2 = ((ref_minus_ref r1 r2) = [])

let (@<=) r1 r2 = ref_prefixeq_ref r1 r2

let ref_prefix_ref r1 r2 = ( (ref_prefixeq_ref r1 r2) && not (r1 = r2) )

let (@<) r1 r2 = ref_prefix_ref r1 r2

let rec trace base r =
	match r with
	| [] -> base
	| "THIS" :: rs -> trace base rs
	| "ROOT" :: rs -> trace [] rs
	| "PARENT" :: rs -> if base = [] then error 102 else trace (prefix base) rs
	| id :: rs -> trace (ref_plus_id base id) rs

let (@<<) base r = trace base r

let simplify r = trace [] r

let (!!) r = simplify r


(** store functions **)

let rec find s r : _value =
	match (s, r) with
	| _, [] -> Val (Store s)
	| [], _ -> Undefined
	| (ids,vs) :: tail, id::rs ->
		if ids = id then
			if rs = [] then Val vs
			else
				match vs with
				| Store child -> find child rs
				| _ -> Undefined
		else find tail r

and resolve s ns r =
	match r with
	| "ROOT" :: rs -> ([], find s !!rs)
	| "PARENT" :: rs -> if ns = [] then error 101 else (prefix ns, find s !!((prefix ns) @++ rs))
	| "THIS" :: rs -> (ns, find s !!(ns @++ rs))
	| _ ->
		if ns = [] then ([], find s !!r)
		else
			let v = find s (ns @<< r) in
			match v with
			| Undefined -> resolve s (prefix ns) r
			| _ -> (ns, v)

and resolve_link s ns r lr =
	match lr with
	| Link rl -> get_link s ns r rl SetRef.empty
	| _ -> error 104

and get_link s ns r rl acc =
	if SetRef.exists (fun rx -> rx = rl) acc then error 105
	else 
		match (resolve s ns rl) with
		| nsp, vp ->
			(
				let rp = nsp @++ rl in
				match vp with
				| Val (Link rm) -> get_link s (prefix rp) r rm (SetRef.add rp acc)
				| _ -> if rp @<= r then error 106 else (rp, vp)
			)

and put s id v : store =
	match s with
	| [] -> (id, v) :: []
	| (ids,vs) :: tail ->
		if ids = id then
			match vs, v with
			| Store dest, Store src -> (id, Store (copy dest src [])) :: tail
			| _,_ -> (id, v) :: tail
		else (ids,vs) :: put tail id v

and copy dest src pfx : store =
	match src with
	| [] -> dest
	| (ids,vs) :: tail -> copy (bind dest (pfx @+. ids) vs) tail pfx

and bind s r v : store =
	match r with
	| [] -> error 3
	| id :: rs ->
		if rs = [] then put s id v
		else
			match s with
			| [] -> error 2
			| (ids,vs) :: tail ->
				if ids = id then
					match vs with
					| Store child -> (id, Store (bind child rs v)) :: tail
					| _ -> error 1
				else (ids,vs) :: bind tail r v

and inherit_proto s ns proto r : store =
	match resolve s ns proto with
	| _, Val (Store vp) -> copy s vp r
	| _, Val (Link rq) ->
		(
			match resolve_link s ns r (Link rq) with
			| _, Val (Store vq) -> copy s vq r
			| _, _ -> error 7
		)
	| _, _ -> error 6

and replace_link s ns cell nss =
	match cell with
	| id, v ->
		(
			let rp = ns @+. id in
			match v with
			| Link rl ->
				(
					match resolve_link s nss rp (Link rl) with
					| _, Undefined -> error 110
					| nsp, Val vp  ->
						(
							let sp = bind s rp vp in
							match vp with
							| Store ssp -> accept sp rp ssp nsp
							| _         -> sp
						)
				)
			| Store vs -> accept s rp vs rp
			| _ -> s
		)
		
and accept s ns ss nss =
	match ss with
	| []      -> s
	| c :: sp ->
		let sq = replace_link s ns c nss in
		accept sq ns sp nss

