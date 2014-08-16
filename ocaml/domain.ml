open Common

(*******************************************************************
 * semantics primary and secondary domains
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
and action         = reference * parameter_type list * int * conditions * effect list
and parameter_type = ident * Syntax._type
and cost           = int
and conditions     = _constraint
and effect         = reference * basic

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


(*******************************************************************
 * convert reference (list of string) to string
 *******************************************************************)
let string_of_ref r = "$." ^ String.concat "." r

let (!^) r = string_of_ref r


(*******************************************************************
 * convert the semantics domain to YAML
 *******************************************************************)
let rec yaml_of_store s = yaml_of_store1 s ""

and yaml_of_store1 s tab =
	match s with
	| [] -> "{}"
	| (ids, vs) :: [] -> yaml_of_cell ids vs tab
	| (ids, vs) :: tail -> (yaml_of_cell ids vs tab) ^ "\n" ^ (yaml_of_store1 tail tab)

and yaml_of_cell ids vs tab =
	let name = tab ^ ids ^ ": " in
	let value =
		match vs with
		| Link lr       -> "link " ^ !^lr
		| Basic basic   -> yaml_of_basic basic
		| Store []      -> yaml_of_store1 [] (tab ^ "  ")
		| Store child   -> "\n" ^ yaml_of_store1 child (tab ^ "  ")
		| Global global -> yaml_of_constraint global (tab ^ "  ")
		| Action action -> yaml_of_action action (tab ^ "  ")
	in
	name ^ value

and yaml_of_vec vec =
	match vec with
	| [] -> ""
	| head :: tail ->
		let v = yaml_of_basic head in
		if tail = [] then v else v ^ "," ^ (yaml_of_vec tail)

and yaml_of_basic v =
	match v with
	| Boolean b -> string_of_bool b
	| Number (Int i) -> string_of_int i
	| Number (Float f) -> string_of_float f
	| String s -> s
	| Null -> "null"
	| Vector vec -> "[" ^ (yaml_of_vec vec) ^ "]"
	| Ref r -> string_of_ref r

(*******************************************************************
 * convert the semantics domains to JSON
 *******************************************************************)
and json_of_store s = "{" ^ (json_of_store1 s) ^ "}"

and json_of_store1 s =
	match s with
	| []                -> ""
	| (ids, vs) :: []   -> json_of_cell ids vs
	| (ids, vs) :: tail -> (json_of_cell ids vs) ^ "," ^ json_of_store1 tail

and json_of_cell id v = "\"" ^ id ^ "\":" ^ (json_of_value v)

and json_of_value v =
	match v with
	| Link lr -> "\"link " ^(string_of_ref lr) ^ "\""
	| Basic basic -> json_of_basic basic
	| Store child -> "{" ^ json_of_store1 child ^ "}"
	| Global global -> json_of_constraint global
	| Action action -> json_of_action action

and json_of_basic v =
	match v with
	| Boolean b -> string_of_bool b
	| Number (Int i) -> string_of_int i
	| Number (Float f) -> string_of_float f
	| String s -> "\"" ^ s ^ "\""
	| Null -> "null"
	| Vector vec -> "[" ^ (json_of_vec vec) ^ "]"
	| Ref r -> "\"" ^ (string_of_ref r) ^ "\""

and json_of_vec vec =
	match vec with
	| [] -> ""
	| head :: tail ->
		let h = json_of_basic head in
		if tail = [] then h else h ^ "," ^ (json_of_vec tail)

(*******************************************************************
 * convert a constraint to JSON
 *******************************************************************)
and json_of_constraint c =
	match c with
	| Eq (r, v)      -> json_of_equal r v
	| Ne (r, v)      -> json_of_not_equal r v
	| Not _          -> json_of_negation c
	| Imply (c1, c2) -> json_of_implication c1 c2
	| And cs         -> json_of_conjunction cs
	| Or cs          -> json_of_disjunction cs
	| In (r, vec)    -> json_of_membership r vec
	| True           -> "true"
	| False          -> "false"

and json_of_conjunction cs =
	(List.fold_left (fun s c -> s ^ "," ^ (json_of_constraint c)) "[\"and\"" cs) ^ "]"

and json_of_disjunction cs =
	(List.fold_left (fun s c -> s ^ "," ^ (json_of_constraint c)) "[\"or\"" cs) ^ "]"

and json_of_equal r bv =
	"[\"=\",\"" ^ !^r ^ "\"," ^ (json_of_basic bv) ^ "]"

and json_of_not_equal r bv =
	"[\"!=\",\"" ^ !^r ^ "\"," ^ (json_of_basic bv) ^ "]"

and json_of_negation c =
	"[\"not\"," ^ (json_of_constraint c) ^ "]"

and json_of_implication c1 c2 =
	"[\"imply\"," ^ (json_of_constraint c1) ^ "," ^ (json_of_constraint c2) ^ "]"

and json_of_membership r v =
	"[\"in\",\"" ^ !^r ^ "\",[" ^ (json_of_vec v) ^ "]]"

(*******************************************************************
 * convert a constraint to YAML
 *******************************************************************)

and yaml_of_constraint c tab = json_of_constraint c

(*******************************************************************
 * convert an action to JSON
 *******************************************************************)
and json_of_effect (r, bv) =
	"[\"=\",\"" ^ !^r ^ "\"," ^ (json_of_basic bv) ^ "]"

and json_of_effects effs =
	(List.fold_left (fun acc e -> acc ^ "," ^ (json_of_effect e)) "[\"effects\"" effs) ^ "]"

and json_of_conditions cond = "[\"conditions\"," ^ (json_of_constraint cond) ^ "]"

and json_of_cost cost = "[\"cost\"," ^ (string_of_int cost) ^ "]"

and json_of_parameter (id, t) = "[\"" ^ id ^ "\",\"" ^ (Syntax.string_of_type t) ^ "\"]"

and json_of_parameters params =
	(List.fold_left (fun acc p -> acc ^ "," ^ (json_of_parameter p)) "[\"parameters\"" params) ^ "]"

and json_of_action (name, params, cost, conds, effs) =
	"[\"" ^ !^name ^ "\"," ^ (json_of_parameters params) ^ "," ^ (json_of_conditions conds) ^
	"," ^ (json_of_effects effs) ^ "]"

(*******************************************************************
 * convert an action to YAML
 *******************************************************************)

and yaml_of_action (name, params, cost, conds, effs) tab =
	"\n" ^
	tab ^ "name: " ^ !^name ^ "\n" ^
	tab ^ "parameters: " ^ (yaml_of_parameters params (tab ^ "  ")) ^ "\n" ^
	tab ^ "cost: " ^ (string_of_int cost) ^ "\n" ^
	tab ^ "conditions: " ^ (yaml_of_constraint conds (tab ^ "  ")) ^ "\n" ^
	tab ^ "effects: " ^ (yaml_of_effects effs (tab ^ "  "))

and yaml_of_parameters params tab =
	List.fold_left (fun acc p -> acc ^ "\n" ^ (yaml_of_parameter p tab)) "" params

and yaml_of_parameter (id, t) tab = tab ^ id ^ ": " ^ (Syntax.string_of_type t)

and yaml_of_effects effs tab =
	List.fold_left (fun acc eff -> acc ^ "\n" ^ (yaml_of_effect eff tab)) "" effs

and yaml_of_effect (r, bv) tab = tab ^ !^r ^ ": " ^ (json_of_basic bv)


(*******************************************************************
 * Flat-Store domain
 *******************************************************************)

type flatstore = value MapRef.t

let static_object = Store []

let normalise (s: store) : flatstore =
	let rec visit (s: store) (ns: reference) (fs: flatstore) : flatstore =
		match s with
		| [] -> fs
		| (id, v) :: tail ->
			let r = ns @+. id in
			let fss =
				match v with
				| Store child                  -> visit child r (MapRef.add r static_object fs)
				| Action (_, ps, c, pre, post) -> MapRef.add r (Action (r, ps, c, pre, post)) fs
				| _                            -> MapRef.add r v fs
			in
			visit tail ns fss
	in
	visit s [] MapRef.empty

let string_of_flatstore (fs: flatstore) : string =
	MapRef.fold (fun r v acc -> acc ^ !^r ^ ": " ^ (json_of_value v) ^ "\n") fs ""


(*******************************************************************
 * set of values
 *******************************************************************)

module SetValue = Set.Make ( struct
	type t = value
	let compare = Pervasives.compare
end )

let string_of_setvalue (sv: SetValue.t) : string =
	SetValue.fold (fun v s -> s ^ (json_of_value v) ^ ";") sv ""


(*******************************************************************
 * parameters
 *******************************************************************)

type ground_parameters = basic MapStr.t

(**
 * substitute each left-hand side reference with a reference as
 * specified in the parameters table
 *)
let substitute_parameter_of_reference (r: reference) (params: ground_parameters) : reference =
	match r with
	| id :: tail ->
		if MapStr.mem id params then
			match MapStr.find id params with
			| Ref r1 -> r1 @++ tail
			| _      -> error 517 (* cannot replace left-hand side reference with a non-reference value *)
		else r
	| _ -> r

(**
 * substitute each right-hand side reference of basic value
 * with a value as specified in the parameters table
 *)
let substitute_parameter_of_basic_value (bv: basic) (params: ground_parameters) : basic =
	match bv with
	| Ref (id :: tail) ->
		if MapStr.mem id params then
			match MapStr.find id params with
			| Ref r1            -> Ref (r1 @++ tail)
			| v1 when tail = [] -> v1
			| _                 -> error 518
		else bv
	| _ -> bv

