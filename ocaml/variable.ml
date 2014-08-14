open Common
open Sfdomain

(** variable := name * index * values * init * goal **)
type t = { name: reference; index: int; values: value array; init: value; goal: value }

type ts = { map: t MapRef.t; arr: t array }

let make r i vals init goal = { name = r; index = i; values = vals; init = init; goal = goal }

let make_ts map arr = { map = map; arr = arr }

let sort ts = Array.fast_sort (fun v1 v2 -> v1.index - v2.index) ts.arr

let mem r (vars: ts) = MapRef.mem r vars.map

let find r (vars: ts) = MapRef.find r vars.map

let values_of r (vars: ts) = if MapRef.mem r vars.map then (MapRef.find r vars.map).values else [| |]

let total vars = Array.length vars.arr

let iter f vars = Array.iter f vars.arr

let iteri_values f var = Array.iteri f var.values

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

let string_of_values =
	Array.fold_left (fun acc v -> acc ^ (json_of_value v) ^ ";") ""

let string_of_variable var =
	!^(var.name) ^ "|" ^ string_of_int(var.index) ^ "|" ^ (string_of_values var.values) ^ "|" ^
	(json_of_value var.init) ^ "|" ^ (json_of_value var.goal)

let string_of_variables vars =
	Array.fold_left (fun acc var -> (string_of_variable var) ^ "\n" ^ acc) "" vars.arr

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
