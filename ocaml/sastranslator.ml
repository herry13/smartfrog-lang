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
let create_flat_store s =
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

let create_type_table e fs = (* e: type-environment, fs: flat-store *)
	let t_obj = (Sfsyntax.TBasic Sfsyntax.TObject) in
	let values_of_type t table =
		if TypeTable.mem t table then TypeTable.find t table else SetValue.empty
	in
	let add_value t v table = 
		TypeTable.add t (SetValue.add v (values_of_type t table)) table
	in
	let rec add_object t v t_next table =
		let table1 = add_value (Sfsyntax.TBasic t) v table in
		let table2 = add_value (Sfsyntax.TRef t) v table1 in
		match t_next with
		| Sfsyntax.TObject -> add_value t_obj v table2
		| Sfsyntax.TSchema (sid, super) -> add_object t_next v super table2
		| _ -> error 501 (* super-type must be another schema or an object *)
	in
	FlatStore.fold (
		fun r v table ->
			match Sftype.type_of e r with
			| Sftype.Undefined -> error 502
			| Sftype.Type Sfsyntax.TBasic Sfsyntax.TSchema (sid, super) ->
				add_object (Sfsyntax.TSchema (sid, super)) (Basic (Ref r)) super table
			| Sftype.Type t -> add_value t v table
	) fs TypeTable.empty 

let string_of_type_table tt =
	TypeTable.fold (
		fun t v acc -> acc ^ (Sfsyntax.string_of_type t) ^ ": " ^ (string_of_set_value v) ^ "\n"
	) tt ""

let sas ast =
	(* type system *)
	let e = Sftype.sfpSpecification ast in
	let s = Sfvaluation.sfpSpecification ast in
	let fs = create_flat_store s in
	let tt = create_type_table e fs in
	"=== flat-store ===\n" ^ (string_of_flat_store fs) ^
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
