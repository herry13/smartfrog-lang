(*******************************************************************
 * semantics domain
 *******************************************************************)
type number = Int of int
            | Float of float
and vector  = basic list
and basic   = Bool of bool
            | Num of number
            | Str of string
            | Null
            | Ref of string list
            | Vec of vector
and value   = Basic of basic
            | Store of store
and _value  = Val of value
            | Undefined
and cell    = string * value
and store   = cell list

(*******************************************************************
 * helpers
 *******************************************************************)

(* reference of main component *)
let ref_of_main = ["sfConfig"]

(***
 * receive and print semantics error message
 * @code int error code
 ***)
let failure code =
	prerr_string ("[err" ^ string_of_int(code) ^ "]\n");
	exit code

(*******************************************************************
 * semantics algebras
 *******************************************************************)

let rec prefix r =
	match r with
	| [] -> []
	| head::tail -> if tail = [] then [] else head :: (prefix tail)

let rec ref_plus_id r id =
	match r with
	| [] -> [id]
	| head::tail -> head :: (ref_plus_id tail id)

let id_plus_id id1 id2 = id1 :: id2 :: []
 
let rec id_plus_ref id r = id :: r

let rec ref_plus_ref r1 r2 =
	match r1 with
	| [] -> r2
	| id::rs -> if r2 = [] then r1 else id :: (ref_plus_ref rs r2)

let rec ref_minus_ref r1 r2 =
	if r1 = [] then []
	else if r2 = [] then r1
	else if (List.hd r1) = (List.hd r2) then ref_minus_ref (List.tl r1) (List.tl r2)
	else r1

let rec (==) r1 r2 : bool =
	if r1 = [] then
		if r2 = [] then true else false
	else if r2 = [] then false
	else if (List.hd r1) = (List.hd r2) then (List.tl r1) == (List.tl r2)
	else false

let rec (<=) r1 r2 : bool = if (ref_minus_ref r1 r2) = [] then true else false;;

let rec (<) r1 r2 : bool = ( (r1 <= r2) && not (r1 == r2) )

let rec trace base r =
	match r with
	| [] -> base
	| "THIS" :: rs -> trace base rs
	| "ROOT" :: rs -> trace [] rs
	| "PARENT" :: rs -> if base = [] then failure 102 else trace (prefix base) rs
	| id :: rs -> trace (ref_plus_id base id) rs

let simplify r = trace [] r

let rec find s r : _value =
	match (s, r) with
	| _, [] -> Val (Store s)
	| [], _ -> Undefined
	| (ids,vs)::tail, id::rs ->
		if ids = id then
			if rs = [] then Val vs
			else
				match vs with
				| Store child -> find child rs
				| _ -> Undefined
		else find tail r

and resolve s ns r =
	match r with
	| "ROOT" :: rs -> ([], find s (simplify rs))
	| "PARENT" :: rs -> if ns = [] then failure 101 else (prefix ns, find s (simplify (ref_plus_ref (prefix ns) rs)))
	| "THIS" :: rs -> (ns, find s (simplify (ref_plus_ref ns rs)))
	| _ ->
		if ns = [] then ([], find s (simplify r))
		else
			let v = find s (simplify (ref_plus_ref ns r)) in
			match v with
			| Undefined -> resolve s (prefix ns) r
			| _ -> (ns, v)

and put s id v : store =
	match s with
	| [] -> (id, v) :: []
	| (ids,vs)::tail ->
		if ids = id then
			match vs, v with
			| Store dest, Store src -> (id, Store (copy dest src [])) :: tail
			| _,_ -> (id, v) :: tail
		else (ids,vs) :: put tail id v

and copy dest src pfx : store =
	match src with
	| [] -> dest
	| (ids,vs)::tail -> copy (bind dest (ref_plus_id pfx ids) vs) tail pfx

and bind s r v : store =
	match r with
	| [] -> failure 3
	| id :: rs ->
		if rs = [] then put s id v
		else
			match s with
			| [] -> failure 2
			| (ids,vs)::tail ->
				if ids = id then
					match vs with
					| Store child -> (id, Store (bind child rs v)) :: tail
					| _ -> failure 1
				else (ids,vs) :: bind tail r v

and inherit_proto s ns proto r : store =
	match resolve s ns proto with
	| nsp, Val (Store vp) -> copy s vp r
	| _, _ -> failure 4
