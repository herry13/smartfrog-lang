open Sfdomain

(*******************************************************************
 * helper functions to convert semantics domain to string, YAML
 * or JSON
 *******************************************************************)

(*******************************************************************
 * convert reference (list of string) to string
 *******************************************************************)
let string_of_ref r = "$." ^ String.concat "." r

let (!^) r = string_of_ref r

(*******************************************************************
 * convert a store to YAML
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
 * convert a store to JSON
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
	| Eq e    -> json_of_equal e
	| Ne e    -> json_of_not_equal e
	| Not e   -> json_of_negation e
	| Imply e -> json_of_implication e
	| And e   -> json_of_conjunction e
	| Or e    -> json_of_conjunction e
	| In e    -> json_of_membership e
	| True    -> "true"
	| False   -> "false"

and json_of_conjunction cs =
	(List.fold_left (fun s c -> s ^ "," ^ (json_of_constraint c)) "[\"and\"" cs) ^ "]"

and json_of_disjunction cs =
	(List.fold_left (fun s c -> s ^ "," ^ (json_of_constraint c)) "[\"or\"" cs) ^ "]"

and json_of_equal (r, bv) =
	"[\"=\",\"" ^ !^r ^ "\"," ^ (json_of_basic bv) ^ "]"

and json_of_not_equal (r, bv) =
	"[\"!=\",\"" ^ !^r ^ "\"," ^ (json_of_basic bv) ^ "]"

and json_of_negation e =
	"[\"not\"," ^ (json_of_constraint e) ^ "]"

and json_of_implication (e1, e2) =
	"[\"imply\"," ^ (json_of_constraint e1) ^ "," ^ (json_of_constraint e2) ^ "]"

and json_of_membership (r, v) =
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

and json_of_parameter (id, t) = "[\"" ^ id ^ "\",\"" ^ (Sfsyntax.string_of_type t) ^ "\"]"

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

and yaml_of_parameter (id, t) tab = tab ^ id ^ ": " ^ (Sfsyntax.string_of_type t)

and yaml_of_effects effs tab =
	List.fold_left (fun acc eff -> acc ^ "\n" ^ (yaml_of_effect eff tab)) "" effs

and yaml_of_effect (r, bv) tab = tab ^ !^r ^ ": " ^ (json_of_basic bv)

