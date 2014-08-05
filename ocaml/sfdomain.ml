(*******************************************************************
 * semantics domain
 *******************************************************************)
type number = Int of int
            | Float of float
and vector  = basic list
and basic   = Boolean of bool
            | Number of number
            | String of string
            | Null
            | Vector of vector
            | Ref of string list
            | Link of string list
and value   = Basic of basic
            | Store of store
and _value  = Val of value
            | Undefined
and cell    = string * value
and store   = cell list 

(*******************************************************************
 * helpers
 *******************************************************************)

(***
 * receive and print semantics error message
 * @code int error code
 ***)
let failure code =
	prerr_string ("[err" ^ string_of_int(code) ^ "]\n");
	exit code

(* Module set of reference *)
module SetRef = Set.Make
	(
		struct
			let compare = Pervasives.compare
			type t = string list
		end
	)

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

let ref_prefixeq_ref r1 r2 = (ref_minus_ref r1 r2) = []

let (@<=) r1 r2 = ref_prefixeq_ref r1 r2

let ref_prefix_ref r1 r2 = ( (ref_prefixeq_ref r1 r2) && not (r1 = r2) )

let (@<) r1 r2 = ref_prefix_ref r1 r2

let rec trace base r =
	match r with
	| [] -> base
	| "THIS" :: rs -> trace base rs
	| "ROOT" :: rs -> trace [] rs
	| "PARENT" :: rs -> if base = [] then failure 102 else trace (prefix base) rs
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
	| "PARENT" :: rs -> if ns = [] then failure 101 else (prefix ns, find s !!((prefix ns) @++ rs))
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
	| _ -> failure 104

and get_link s ns r rl acc =
	if SetRef.exists (fun rx -> rx = rl) acc then failure 105
	else 
		match (resolve s ns rl) with
		| nsp, vp ->
			(
				let rp = nsp @++ rl in
				match vp with
				| Val (Basic (Link rm)) -> get_link s (prefix rp) r rm (SetRef.add rp acc)
				| _ -> if rp @<= r then failure 106 else (rp, vp)
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
	| [] -> failure 3
	| id :: rs ->
		if rs = [] then put s id v
		else
			match s with
			| [] -> failure 2
			| (ids,vs) :: tail ->
				if ids = id then
					match vs with
					| Store child -> (id, Store (bind child rs v)) :: tail
					| _ -> failure 1
				else (ids,vs) :: bind tail r v

and inherit_proto s ns proto r : store =
	match resolve s ns proto with
	| _, Val (Store vp) -> copy s vp r
	| _, Val (Basic (Link rq)) ->
		(
			match resolve_link s ns r (Link rq) with
			| _, Val (Store vq) -> copy s vq r
			| _, _ -> failure 7
		)
	| _, _ -> failure 6

and replace_link s ns cell nss =
	match cell with
	| id, v ->
		(
			let rp = ns @+. id in
			match v with
			| Basic (Link rl) ->
				(
					match resolve_link s nss rp (Link rl) with
					| _, Undefined -> failure 110
					| nsp, Val vp ->
						(
							let sp = bind s rp vp in
							match vp with
							| Store ssp -> accept sp rp ssp nsp
							| _ -> sp
						)
				)
			| Store vs -> accept s rp vs rp
			| _ -> s
		)
		
and accept s ns ss nss =
	match ss with
	| [] -> s
	| c :: sp ->
		let sq = replace_link s ns c nss in
		accept sq ns sp nss
