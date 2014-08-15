open Common
open Sfdomain

(** variable := name * index * values * init * goal **)
type t = { name: reference; index: int; values: value array; init: value; goal: value }

type ts = { map: t MapRef.t; arr: t array }


(*****************************************************************
 * variable functions
 *****************************************************************)

let make r i vals init goal = { name = r; index = i; values = vals; init = init; goal = goal }

let iteri_values f var = Array.iteri f var.values

let string_of_values =
	Array.fold_left (fun acc v -> acc ^ (json_of_value v) ^ ";") ""

let string_of_variable var =
	!^(var.name) ^ "|" ^ string_of_int(var.index) ^ "|" ^ (string_of_values var.values) ^ "|" ^
	(json_of_value var.init) ^ "|" ^ (json_of_value var.goal)

let r_dummy = ["!global"];;

let dummy = { name = r_dummy; index = 0; values = [| Basic (Boolean true); Basic (Boolean false) |];
	init = Basic (Boolean false); goal = Basic (Boolean true) };;

let index v = v.index

let name v = v.name

let init v = v.init

let goal v = v.goal

let values v = v.values

let size v = Array.length v.values

let index_of_value v (var: t) : int =
	let l = Array.length var.values in
	let rec iter i =
		if i >= l then -1
		else if var.values.(i) = v then i
		else iter (i+1)
	in
	iter 0


(*****************************************************************
 * variables functions
 *****************************************************************)

let sort ts = Array.fast_sort (fun v1 v2 -> v1.index - v2.index) ts.arr

let mem r (vars: ts) = MapRef.mem r vars.map

let find r (vars: ts) = MapRef.find r vars.map

let values_of r (vars: ts) = if MapRef.mem r vars.map then (MapRef.find r vars.map).values else [| |]

let total vars = Array.length vars.arr

let iter f vars = Array.iter f vars.arr

let intersection_with_values r vec (vars: ts) =
	let var = find r vars in
	let temp = Array.fold_left (fun acc v1 ->
			match v1 with
			| Basic v2 -> if List.mem v2 vec then v1 :: acc else acc
			| _        -> v1 :: acc
		) [] var.values
	in
	let var1 = { name = var.name; index = var.index; values = Array.of_list temp; init = var.init; goal = var.goal } in
	vars.arr.(var1.index) <- var1;
	{ map = MapRef.add r var1 vars.map; arr = vars.arr }

let intersection_with_value r v (vars: ts) =
	let var = find r vars in
	let l = Array.length var.values in
	let rec exists i =
		if i >= l then false
		else if var.values.(i) = v then true
		else exists (i+1)
	in
	let temp = if exists 0 then [| v |] else [| |] in
	let var1 = { name = var.name; index = var.index; values = temp; init = var.init; goal = var.goal } in
	vars.arr.(var1.index) <- var1;
	{ map = MapRef.add r var1 vars.map; arr = vars.arr }

let remove_value_from r v (vars: ts) =
	let var = find r vars in
	let temp = Array.fold_left (fun acc v1 -> if v1 = v then acc else v1 :: acc) [] var.values in
	let var1 = { name = var.name; index = var.index; values = Array.of_list temp; init = var.init; goal = var.goal } in
	vars.arr.(var1.index) <- var1;
	{ map = MapRef.add r var1 vars.map; arr = vars.arr }

let remove_values_from (r: reference) (vec: vector) (vars: ts) =
	let var = find r vars in
	let temp = Array.fold_left (fun acc v1 ->
			match v1 with
			| Basic v2 -> if List.mem v2 vec then acc else v1 :: acc
			| _        -> v1 :: acc
		) [] var.values
	in
	let var1 = { name = var.name; index = var.index; values = Array.of_list temp; init = var.init; goal = var.goal } in
	vars.arr.(var1.index) <- var1;
	{ map = MapRef.add r var1 vars.map; arr = vars.arr }

let string_of_variables vars =
	Array.fold_left (fun acc var -> (string_of_variable var) ^ "\n" ^ acc) "" vars.arr


(* let make_ts map arr = { map = map; arr = arr } *)
let make_ts (env_0: Sftype.env) (fs_0: flatstore) (env_g: Sftype.env) (fs_g: flatstore) (tvalues: Sftype.typevalue) : ts =
	let type_of_var r =
		match (Sftype.type_of r env_0), (Sftype.type_of r env_g) with
		| t1, t2 when t1 = t2 -> t1
		| _, _                -> error 504  (* incompatible type between init & goal *)
	in
	let map0 = MapRef.add r_dummy dummy MapRef.empty in
	let arr0 = [dummy] in
	let (map1, total, arr1) = MapRef.fold (
			fun r v (map, i, arr) ->
				if MapRef.mem r map then error 505;
				match type_of_var r with
				| Sfsyntax.TBasic Sfsyntax.TAction
				| Sfsyntax.TBasic Sfsyntax.TGlobal -> (map, i, arr)
				| Sfsyntax.TBasic Sfsyntax.TObject
				| Sfsyntax.TBasic Sfsyntax.TSchema (_, _) ->
					let v = static_object in
					let var = make r i [|v|] v v in
					let map1 = MapRef.add r var map in
					let arr1 = var :: arr in
					(map1, i+1, arr1)
				| t ->
					let values = Array.of_list (SetValue.elements (Sftype.values_of t tvalues)) in
					let init = MapRef.find r fs_0 in
					let goal = MapRef.find r fs_g in
					let var = make r i values init goal in
					let map1 = MapRef.add r var map in
					let arr1 = var :: arr in
					(map1, i+1, arr1)
		) fs_0 (map0, 1, arr0)
	in
	let vars = { map = map1; arr = (Array.of_list arr1) } in
	sort vars;
	vars

