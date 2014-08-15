open Printf
open Common
open Sfdomain

type t = reference * basic MapStr.t * cost * basic MapRef.t * basic MapRef.t;;

let json_of_parameters (ps: basic MapStr.t) : string =
	let buf = Buffer.create 42 in
	Buffer.add_char buf '{';
	MapStr.iter (fun id v ->
		Buffer.add_string buf id;
		Buffer.add_string buf "\":";
		Buffer.add_string buf (json_of_value (Basic v));
		Buffer.add_string buf ",\"";
	) ps;
	if Buffer.length buf <= 1 then "{}"
	else (Buffer.sub buf 0 ((Buffer.length buf) - 1)) ^ "}"

let encode_name (name: reference) (ps: basic MapStr.t) : string =
	!^name ^ " " ^ (json_of_parameters ps)

let json_of_preconditions (pre: basic MapRef.t) : string =
	let buf = Buffer.create 42 in
	MapRef.iter (fun r v ->
		Buffer.add_string buf ",\"";
		Buffer.add_string buf !^r;
		Buffer.add_string buf "\":";
		Buffer.add_string buf (json_of_value (Basic v))
	) pre;
	let s = Buffer.contents buf in
	if s = "" then "{}" else "{" ^ (String.sub s 1 ((String.length s) -1)) ^ "}";;

let json_of_effects = json_of_preconditions;;

let json_of (a: t) : string =
	let (name, ps, cost, pre, eff) = a in
	sprintf "{\"name\":\"%s\",\"parameters\":%s,\"cost\":%d,\"conditions\":{%s},\"effects\":{%s}}"
		!^name (json_of_parameters ps) cost (json_of_preconditions pre) (json_of_effects eff);;

let json_of_actions (actions: t list) : string =
	match actions with
	| [] -> "[]"
	| act :: [] -> "[" ^ (json_of act) ^ "]"
	| act :: acts -> "[" ^ (json_of act) ^ (List.fold_left (fun s a -> s ^ "," ^ (json_of a)) "" acts) ^ "]"

(* convert a list of (identifier => type) to a list of maps of (identifier => value) *)
let create_parameter_table params name (tvalues: Sftype.typevalue) =
	let table1 = MapStr.add "this" [Ref (prefix name)] MapStr.empty in
	let table2 = List.fold_left (fun table (id, t) ->
			let values = SetValue.fold (fun v acc ->
					match v with
					| Basic bv -> bv :: acc
					| _ -> acc
				) (Sftype.values_of t tvalues) []
			in
			MapStr.add id values table
		) table1 params
	in
	MapStr.fold (fun id values acc1 ->
		List.fold_left (fun acc2 v ->
			if acc1 = [] then (MapStr.add id v MapStr.empty) :: acc2
			else List.fold_left (fun acc3 table -> (MapStr.add id v table) :: acc3) acc2 acc1
		) [] values
	) table2 []

let equals_to_map = fun map c ->
	match c with
	| Eq (r, v) -> MapRef.add r v map
	| _         -> error 520

(** for each clause of global constraints DNF, create a dummy action **)
let create_global_actions (global: _constraint) (acc: t list) : t list =
	let pre = MapRef.add Variable.r_dummy (Boolean false) MapRef.empty in
	let eff = MapRef.add Variable.r_dummy (Boolean true) MapRef.empty in
	let params = MapStr.empty in
	let counter = ref 0 in
	match global with
	| True  -> acc
	| Or cs -> List.fold_left (fun (acc1: t list) c ->
			let name = ["!global" ^ (string_of_int !counter)] in
			counter := !counter + 1;
			match c with
			| And css ->
				let pre1 = List.fold_left equals_to_map pre css in
				(name, params, 0, pre1, eff) :: acc1
			| Eq _    -> (name, params, 0, pre, eff) :: acc1
			| _       -> error 523
		) acc cs
	| And cs ->
		let name = ["!global"] in
		let pre1 = List.fold_left equals_to_map pre cs in
		[(name, params, 0, pre1, eff)]
	| _      -> error 524

(**
 * Compile simple implication of global constraints by modifying precondition of actions.
 * @param pre        precondition of an action
 * @param eff        effect of an action
 * @param vars       data structure that holds the variables
 * @param g_implies  a list of simple implication formula
 *
 * @return a list of preconditions, each of which is for an action
 *)
let compile_simple_implication pre eff vars g_implies =
	let modified = ref false in
	let rec iter lpre cs =
		if lpre = [] then lpre else
		match cs with
		| [] -> lpre
		| Imply (Eq (rp, vp), Eq (rc, vc)) :: css ->
			let compile map pre =
				if (MapRef.mem rp map) && (MapRef.find rp map) = vp then   (* mem |= premise *)
					if (MapRef.mem rc pre) then (
						if (MapRef.find rc pre) <> vc then []              (* pre not|= conclusion *)
						else [pre]                                         (* pre |= conclusion *)
					) else (
						modified := true;       (* add extra precondition such that pre |= conclusion *)
						[MapRef.add rc vc pre]
					)
				else if (MapRef.mem rc map) && (MapRef.find rc map) <> vc then   (* mem not|= conclusion *)
					if (MapRef.mem rp pre) && (MapRef.find rp pre) = vp then []  (* pre |= premise *)
					else (
						Array.fold_left (fun acc v ->
							match v with
					     	| Basic vx when vx <> vp -> (
									if MapRef.mem rp pre then (
										if (MapRef.find rp pre) <> vp then pre :: acc  (* pre |= premise = true *)
										else acc                                       (* pre |= premise = false *)
									) else (
										modified := true;    (* add extra precondition such that pre |= conclusion *)
										(MapRef.add rp vx pre) :: acc
									)
								)
							| _ -> acc
						) [] (Variable.values_of rp vars)
					)
				else [pre]
			in
			let lpre1 = List.fold_left (fun lpre1 pre ->
					let lpre2 = compile pre pre in
					List.fold_left (fun lpre3 pre -> List.append (compile eff pre) lpre3) [] lpre2
				) [] lpre
			in
			iter lpre1 css
		| _ -> error 1001
	in
	let rec compile lpre =
		let lpre1 = iter lpre g_implies in
		if !modified then (
			modified := false;
			compile lpre1
		) else lpre1
	in
	compile [pre]
	;;

(**
 * Ground an SFP action.
 * 1. substitute the parameters
 * 2. convert the preconditions into the DNF-formula
 * 3. foreach DNF clause, copy the original action and set the clause as the preconditions
 * 4. apply simple implication compilation
 *
 * returns a list of grounded actions
 *)
let ground_action_of (name, params, cost, pre, eff) env vars typevalue dummy (g_implies : _constraint list) : t list =
	let param_tables = create_parameter_table params name typevalue in
	List.fold_left (fun acc1 ps ->
		let eff1 = List.fold_left (fun acc (r, bv) ->
				let r1 = substitute_parameter_of_reference r ps in
				let bv1 = substitute_parameter_of_basic_value bv ps in
				MapRef.add r1 bv1 acc
			) MapRef.empty eff
		in
		let eff2 = if dummy then MapRef.add Variable.r_dummy (Boolean false) eff1 else eff1 in
		let pre1 = Constraint.substitute_parameters_of pre ps in
		let pre2 = if dummy then MapRef.add Variable.r_dummy (Boolean true) MapRef.empty else MapRef.empty in
		match Constraint.dnf_of pre1 vars env with
		| False -> acc1
		| Or cs -> List.fold_left (fun acc2 c ->
				let pre3 =
					match c with
					| Eq (r, v) -> MapRef.add r v pre2
					| And css   -> List.fold_left equals_to_map pre2 css
					| _         -> error 521
				in
				List.fold_left (fun acc3 pre4 ->
					(name, ps, cost, pre4, eff2) :: acc3
				) acc2 (compile_simple_implication pre3 eff2 vars g_implies)
			) acc1 cs
		| And css   ->
			let pre3 = List.fold_left equals_to_map pre2 css in
			List.fold_left (fun acc2 pre4 ->
				(name, ps, cost, pre4, eff2) :: acc2
			) acc1 (compile_simple_implication pre3 eff2 vars g_implies)
		| Eq (r, v) ->
			let pre3 = MapRef.add r v pre2 in
			List.fold_left (fun acc2 pre4 ->
				(name, ps, cost, pre4, eff2) :: acc2
			) acc1 (compile_simple_implication pre3 eff2 vars g_implies)
		| True      ->
			List.fold_left (fun acc2 pre3 ->
				(name, ps, cost, pre3, eff2) :: acc2
			) acc1 (compile_simple_implication pre2 eff2 vars g_implies)
		| _         -> error 522
	) [] param_tables

(* ground a set of actions - returns a list of grounded actions *)
let ground_actions (env: Sftype.env) (vars: Variable.ts) (tvalues: Sftype.typevalue)
                   (global: _constraint) (g_implies : _constraint list) : t list =
	print_endline (json_of_constraint (And g_implies));
	let add_dummy = not (global = True) in
	let actions : t list = create_global_actions global [] in
	SetValue.fold (
		fun v gactions ->
			match v with
			| Action a -> List.append (ground_action_of a env vars tvalues add_dummy g_implies) gactions
			| _        -> gactions
	) (Sftype.values_of (Sfsyntax.TBasic Sfsyntax.TAction) tvalues) actions
