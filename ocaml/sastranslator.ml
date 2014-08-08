open Sfdomain

(*******************************************************************
 * Modules and functions to convert SFP to FD-SAS.
 * There are three modules:
 * - FlatStore : a map which is the flat version of store domain
 * - SetValue  : a set of values (of particular type)
 * - TypeTable : a map of type -> values
 *******************************************************************)

(*******************************************************************
 * flat-store
 *******************************************************************)

(** flat-store domain *)
module FlatStore = Map.Make
	(
		struct
			type t = reference
			let compare = Pervasives.compare
		end
	)

(**
 * Convert a store to a flat_store
 *
 * @param s store to be flatten
 * @return a flat_store
 *)
let normalise s =
	let rec visit s ns fs =
		match s with
		| []              -> fs
		| (id, v) :: tail ->
			let r = ns @+. id in
			let fss =
				match v with
				| Store child -> visit child r (FlatStore.add r (Store []) fs)
				| _           -> FlatStore.add r v fs
			in
			visit tail ns fss
	in
	visit s [] FlatStore.empty

(**
 * convert a flat-store to string
 *)
let string_of_flat_store fs =
	FlatStore.fold (
		fun r v acc -> acc ^ !^r ^ ": " ^ (Sfdomainhelper.json_of_value v) ^ "\n"
	) fs ""


(*******************************************************************
 * set-value
 *******************************************************************)

module SetValue = Set.Make
	(
		struct
			type t = value
			let compare = Pervasives.compare
		end
	)

let string_of_set_value sv =
	SetValue.fold (
		fun v acc -> acc ^ (Sfdomainhelper.json_of_value v) ^ ";"
	) sv ""

(*******************************************************************
 * type-table
 *******************************************************************)

module TypeTable = Map.Make
	(
		struct
			type t = Sfsyntax._type
			let compare = Pervasives.compare
		end
	)

let t_bool = Sfsyntax.TBasic Sfsyntax.TBool
let t_num = Sfsyntax.TBasic Sfsyntax.TNum
let t_str = Sfsyntax.TBasic Sfsyntax.TStr
let t_obj = Sfsyntax.TBasic Sfsyntax.TObject
let t_action = Sfsyntax.TBasic Sfsyntax.TAction
let t_global = Sfsyntax.TBasic Sfsyntax.TGlobal

let values_of_type t typetable =
	if TypeTable.mem t typetable then TypeTable.find t typetable else SetValue.empty

let add_value_of_type t v typetable = 
	TypeTable.add t (SetValue.add v (values_of_type t typetable)) typetable

(** group action effects' values based on their type **)
let add_action_values e typetable =
	let actions = values_of_type t_action typetable in
	let add_effect_values =
		List.fold_left (
			fun acc (r, v) ->
				match v with
				| Boolean _ -> add_value_of_type t_bool (Basic v) acc
				| Number  _ -> add_value_of_type t_num (Basic v) acc
				| String  _ -> add_value_of_type t_str (Basic v) acc
				| Vector  _ -> error 503 (* TODO *)
				| _         -> acc
		) 
	in
	SetValue.fold (
		fun v typetable ->
			match v with
			| Action (n, ps, c, pre, eff) -> add_effect_values typetable eff
			| _ -> typetable
	) actions typetable

(** group values based on their type *)
let create_type_table env_0 fs_0 env_g fs_g = (* e: type-environment, fs: flat-store *)
	let null = Basic Null in
	let rec add_object t v t_next typetable =
		let typetable1 = add_value_of_type (Sfsyntax.TBasic t) v typetable in
		let typetable2 = add_value_of_type (Sfsyntax.TRef t) v typetable1 in
		let typetable3 = add_value_of_type (Sfsyntax.TRef t) null typetable2 in
		match t_next with
		| Sfsyntax.TObject -> add_value_of_type t_obj v typetable3
		| Sfsyntax.TSchema (sid, super) -> add_object t_next v super typetable3
		| _ -> error 501 (* super-type must be another schema or an object *)
	in
	let add_store_values env =
		FlatStore.fold (
			fun r v typetable ->
				match Sftype.type_of env r with
				| Sftype.Undefined -> error 502
				| Sftype.Type Sfsyntax.TBasic Sfsyntax.TSchema (sid, super) ->
					add_object (Sfsyntax.TSchema (sid, super)) (Basic (Ref r)) super typetable
				| Sftype.Type t -> add_value_of_type t v typetable
		)
	in
	let typetable00 = add_store_values env_0 fs_0 TypeTable.empty in
	let typetable01 = add_action_values env_0 typetable00 in
	let typetable10 = add_store_values env_g fs_g typetable01 in
	add_action_values env_g typetable10

let string_of_type_table tt =
	TypeTable.fold (
		fun t v acc -> acc ^ (Sfsyntax.string_of_type t) ^ ": " ^ (string_of_set_value v) ^ "\n"
	) tt ""

(*******************************************************************
 * interface functions for translating SFP to FDR
 *******************************************************************)

let eval_ast ast = (Sftype.sfpSpecification ast, Sfvaluation.sfpSpecification ast)

let fdr ast_0 ast_g =
	(* type system *)
	let (env_0, store_0) = eval_ast ast_0 in
	let (env_g, store_g) = eval_ast ast_g in
	let fs_0 = normalise store_0 in
	let fs_g = normalise store_g in
	let tt = create_type_table env_0 fs_0 env_g fs_g in
	"=== flat-store current ===\n" ^ (string_of_flat_store fs_0) ^
	"=== flat-store desired ===\n" ^ (string_of_flat_store fs_g) ^
	"=== type table ===\n" ^ (string_of_type_table tt)


module Constraint =
	struct
		let eval s = false (* TODO *)
	end

module Action =
	struct
		let applicable s = false (* TODO *)
		let apply s = s (* TODO *)
	end
