open Printf
open Sfdomain

(*******************************************************************
 * Modules and functions to convert SFP to FD-SAS.
 * There are three modules:
 * - FlatStore  : a map which is the flat version of store domain
 * - Values     : a set of values (of particular type)
 * - TypeTable  : a map of type -> values
 * - Constraint : (global) constraints
 * - Action     : actions
 *******************************************************************)

module StrMap = Map.Make(String)

module RefMap = Map.Make
	(
		struct
			type t = reference
			let compare = Pervasives.compare
		end
	)

(*******************************************************************
 * flat-store
 *******************************************************************)

module TEnv =
	struct
		let empty = RefMap.empty;;
		let mem = RefMap.mem;;
		let add = RefMap.add;;
		let find = RefMap.find;;
		let fold = RefMap.fold;;

		let make e = List.fold_left (fun acc (r, t) -> add r t acc) empty e

		let type_of e r = if mem r e then find r e else Sfsyntax.TUndefined

	end

module FlatStore =
	struct
		let empty = RefMap.empty;;
		let mem = RefMap.mem;;
		let add = RefMap.add;;
		let find = RefMap.find;;
		let fold = RefMap.fold;;

		let static_object = Store []

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
						| Store child -> visit child r (add r static_object fs)
						| Action (_, ps, c, pre, post) -> add r (Action (r, ps, c, pre, post)) fs
						| _           -> add r v fs
					in
					visit tail ns fss
			in
			visit s [] empty

		let make = normalise

		(**
		 * convert a flat-store to string
		 *)
		let string_of fs = fold (fun r v acc -> acc ^ !^r ^ ": " ^ (Sfdomainhelper.json_of_value v) ^ "\n") fs ""

	end


(*******************************************************************
 * set-value
 *******************************************************************)

module Values =
	struct
		module Set = Set.Make
			( struct
				type t = value
				let compare = Pervasives.compare
			end )

		let empty = Set.empty;;
		let add = Set.add;;
		let fold = Set.fold;;
		let elements = Set.elements;;

		let string_of sv = Set.fold (fun v s -> s ^ (Sfdomainhelper.json_of_value v) ^ ";") sv ""
		
		let string_of_list l = List.fold_left ( fun s v -> s ^ (Sfdomainhelper.json_of_value v) ^ ";") "" l
	end

(*******************************************************************
 * type-table
 *******************************************************************)

(** import types from Sfsyntax module **)
let t_bool   = Sfsyntax.TBasic Sfsyntax.TBool
let t_num    = Sfsyntax.TBasic Sfsyntax.TNum
let t_str    = Sfsyntax.TBasic Sfsyntax.TStr
let t_obj    = Sfsyntax.TBasic Sfsyntax.TObject
let t_action = Sfsyntax.TBasic Sfsyntax.TAction
let t_global = Sfsyntax.TBasic Sfsyntax.TGlobal

module TypeTable =
	struct
		module Map = Map.Make
			( struct
				type t = Sfsyntax._type
				let compare = Pervasives.compare
			end )

		let empty = Map.empty;;
		let mem = Map.mem;;
		let add = Map.add;;
		let find = Map.find;;
		let fold = Map.fold;;

		let values_of t table = if mem t table then find t table else Values.empty

		let add_value t v table = add t (Values.add v (values_of t table)) table

		let string_of table = fold (
				fun t v s -> s ^ (Sfsyntax.string_of_type t) ^ ": " ^ (Values.string_of v) ^ "\n"
			) table ""

		(** group action effects' values based on their type **)
		let add_action_values env table =
			let actions = values_of t_action table in
			let add_effect_values =
				List.fold_left (
					fun acc (r, v) ->
						match v with
						| Boolean _ -> add_value t_bool (Basic v) acc
						| Number  _ -> add_value t_num (Basic v) acc
						| String  _ -> add_value t_str (Basic v) acc
						| Vector  _ -> error 501 (* TODO *)
						| _         -> acc
				)
			in
			Values.fold (
				fun v table1 ->
					match v with
					| Action (n, ps, c, pre, eff) -> add_effect_values table1 eff
					| _                           -> table1
			) actions table

		(**
		 * create a TypeTable by grouping values based on their type
		 *
		 * @param env_0  type environment of initial state
		 * @param fs_0   flat-store of initial state
		 * @param env_g  type environment of goal state
		 * @param fs_g   flat-store of goal state
		 * @return a type-table
		 *)
		let make env_0 fs_0 env_g fs_g = (* e: type-environment, fs: flat-store *)
			let null = Basic Null in
			let rec add_object t v t_next table =
				let table1 = add_value (Sfsyntax.TBasic t) v table in
				let table2 = add_value (Sfsyntax.TRef t) v table1 in
				let table3 = add_value (Sfsyntax.TRef t) null table2 in
				match t_next with
				| Sfsyntax.TObject              -> add_value t_obj v table3
				| Sfsyntax.TSchema (sid, super) -> add_object t_next v super table3
				| _                             -> error 502 (* super-type must be another schema or an object-type *)
			in
			let add_store_values env =
				FlatStore.fold (
					fun r v table ->
						match TEnv.type_of env r with
						| Sfsyntax.TUndefined -> error 503
						| Sfsyntax.TBasic Sfsyntax.TSchema (sid, super) ->
							add_object (Sfsyntax.TSchema (sid, super)) (Basic (Ref r)) super table
						| t -> add_value t v table
				)
			in
			let table00 = add_store_values env_0 fs_0 empty in
			let table01 = add_action_values env_0 table00 in
			let table10 = add_store_values env_g fs_g table01 in
			add_action_values env_g table10

	end

(*******************************************************************
 * FDR variables
 *******************************************************************)

type variable = { name: reference; index: int; values: value array; init: value; goal: value }

module Variable =
	struct
		(* variable := name * index * values * init * goal *)
		type t = variable

		let empty = RefMap.empty;;
		let mem = RefMap.mem;;
		let add = RefMap.add;;
		let find = RefMap.find;;
		let fold = RefMap.fold;;

		let values_of r map = if mem r map then (find r map).values else [| |]

		let string_of_values =
			Array.fold_left (fun acc v -> acc ^ (Sfdomainhelper.json_of_value v) ^ ";") ""

		let string_of_variable var =
			!^(var.name) ^ "|" ^ string_of_int(var.index) ^ "|" ^ (string_of_values var.values) ^ "|" ^
			(Sfdomainhelper.json_of_value var.init) ^ "|" ^ (Sfdomainhelper.json_of_value var.goal)

		let string_of_array arr =
			Array.fold_left (fun acc var -> (string_of_variable var) ^ "\n" ^ acc) "" arr

		let r_dummy = ["!global"];;
		let dummy = { name = r_dummy; index = 0; values = [| Basic (Boolean true); Basic (Boolean false) |];
			init = Basic (Boolean false); goal = Basic (Boolean true) };;

		let make env_0 fs_0 env_g fs_g typetable =
			let type_of_var r =
				match (TEnv.type_of env_0 r), (TEnv.type_of env_g r) with
				| t1, t2 when t1 = t2 -> t1
				| _, _                -> error 504  (* incompatible type between init & goal *)
			in
			let map0 = add r_dummy dummy empty in
			let arr0 = [dummy] in
			let (map1, total, arr1) = FlatStore.fold (
					fun r v (map, i, arr) ->
						if mem r map then error 505;
						match type_of_var r with
						| Sfsyntax.TBasic Sfsyntax.TAction
						| Sfsyntax.TBasic Sfsyntax.TGlobal -> (map, i, arr)
						| Sfsyntax.TBasic Sfsyntax.TObject
						| Sfsyntax.TBasic Sfsyntax.TSchema (_, _) ->
							let v = FlatStore.static_object in
							let var = { name = r; index = i; values = [|v|]; init = v; goal = v } in
							let map1 = add r var map in
							let arr1 = var :: arr in
							(map1, i+1, arr1)
						| t ->
							let values = Array.of_list (Values.elements (TypeTable.values_of t typetable)) in
							let init = FlatStore.find r fs_0 in
							let goal = FlatStore.find r fs_g in
							let var = { name = r; index = i; values = values; init = init; goal = goal } in
							let map1 = add r var map in
							let arr1 = var :: arr in
							(map1, i+1, arr1)
				) fs_0 (map0, 1, arr0)
			in
			let arr2 = Array.of_list arr1 in
			Array.fast_sort (fun v1 v2 -> v1.index - v2.index) arr2;
			(map1, arr2)

		let fdr_of (buf: Buffer.t) (var: t) : unit =
			Buffer.add_string buf "\nbegin_variable\nvar";
			Buffer.add_string buf (string_of_int var.index);
			Buffer.add_char buf '_';
			Buffer.add_string buf !^(var.name);
			Buffer.add_string buf "\n-1\n";
			Buffer.add_string buf (string_of_int (Array.length var.values));
			Buffer.add_char buf '\n';
			Array.iteri (fun i v ->
				Buffer.add_string buf (Sfdomainhelper.json_of_value v);
				(* Buffer.add_string buf (string_of_int i); *)
				Buffer.add_char buf '\n';
			) var.values;
			Buffer.add_string buf "end_variable";;

		let fdr_of_variables (buf: Buffer.t) (vars: t array) : unit =
			Buffer.add_char buf '\n';
			Buffer.add_string buf (string_of_int (Array.length vars));
			Array.iter (fun var -> fdr_of buf var;) vars;;

		let index_of (var: t) v : int =
			let l = Array.length var.values in
			let rec iter i =
				if var.values.(i) = v then i
				else if i >= l then error 530
				else iter (i+1)
			in
			iter 0

		let fdr_of_init (buf: Buffer.t) (vars: t array) : unit =
			Buffer.add_string buf "\nbegin_state";
			Array.iter (fun var ->
				Buffer.add_char buf '\n';
				Buffer.add_string buf (string_of_int (index_of var var.init));
			) vars;
			Buffer.add_string buf "\nend_state";;

		let fdr_of_goal (buf: Buffer.t) (vars: t array) use_dummy : unit =
			let tmp = Buffer.create 50 in
			let counter = ref 0 in
			Array.iter (fun var ->
				(**
				 * set the variable's goal, when:
				 * - it has more than one value
				 * - if it is a dummy variable, then the global constraint must be exist
				 *)
				if (Array.length var.values) > 1 && (var.index > 0 || use_dummy) then
				(
					Buffer.add_char tmp '\n';
					Buffer.add_string tmp (string_of_int var.index);
					Buffer.add_char tmp ' ';
					Buffer.add_string tmp (string_of_int (index_of var var.goal));
					counter := !counter + 1;
				)
			) vars;
			Buffer.add_string buf "\nbegin_goal";
			Buffer.add_char buf '\n';
			Buffer.add_string buf (string_of_int !counter);
			Buffer.add_string buf (Buffer.contents tmp);
			Buffer.add_string buf "\nend_goal";;

		let fdr_of_mutex (buf: Buffer.t) (vars: t array) : unit =
			Buffer.add_string buf "\n0";;

	end

(*******************************************************************
 * constraints
 *******************************************************************)

module Constraint =
	struct
		let eval s = false (* TODO *)

		let string_of = Sfdomainhelper.json_of_constraint

		let rec prevail_of r vars =
			if r = [] then error 506
			else if Variable.mem r vars then r
			else prevail_of (prefix r) vars

		(* mode: {1 => Eq, 2 => Ne, 3 => In, 4 => NotIn } *)
		let rec constraints_of_nested r v vars env mode =
			let rec iter r cs =
				let prevail = prevail_of r vars in
				if prevail = r then
					match mode, v with
					| 1, _          -> (Eq (r, v)) :: cs
					| 2, _          -> (Ne (r, v)) :: cs
					| 3, Vector vec -> (In (r, vec)) :: cs
					| 4, Vector vec -> (Not (In (r, vec))) :: cs
					| _             -> error 507
				else
					let cs1 = (Ne (prevail, Null)) :: cs in
					let rs1 = r @-- prevail in
					Array.fold_left (
						fun acc vp ->
							match vp with
							| Basic (Ref rp) ->
								let premise = Eq (prevail, Ref rp) in
								let conclusion = And (iter (rp @++ rs1) []) in
								(Imply (premise, conclusion)) :: acc
							| Basic Null     -> acc
							| _              -> error 508
					) cs1 (Variable.values_of prevail vars)
			in
			And (iter r [])

		(* return false if formula 'c1' negates 'c2' (or vice versa), otherwise true *)
		and incompatible c1 c2 =
			match c1, c2 with
			| Eq (r1, v1), Eq (r2, v2) -> if r1 = r2 then not (v1 = v2) else false
			| Eq (r1, v1), Ne (r2, v2) -> if r1 = r2 then v1 = v2 else false
			| Ne (r1, v1), Eq (r2, v2) -> if r1 = r2 then v1 = v2 else false
			| _                        -> false

		(**
		 * simplify a conjunction formula
		 * - remove duplications
		 * - determine whether the formula is always false
		 *)
		and simplify_conjunction c =
			let rec iter clauses acc =
				match clauses with
				| []         -> And acc
				| c1 :: tail -> 
					if List.exists (fun c2 -> incompatible c1 c2) tail then False
					else if List.exists (fun c2 -> c1 = c2) tail then iter tail acc
					else iter tail (c1 :: acc)
			in
			match c with
			| And clauses -> iter clauses []
			| _           -> error 509

		(**
		 * simplify a disjunction formula
		 * - remove duplications
		 *)
		and simplify_disjunction c =
			let rec iter clauses acc =
				match clauses with
				| [] -> Or acc
				| c1 :: tail ->
					if List.exists (fun c2 -> c1 = c2) tail then iter tail acc
					else iter tail (c1 :: acc)
			in
			match c with
			| Or clauses -> iter clauses []
			| _          -> error 510

		(**
		 * cross product of disjunction clauses of a conjunction formula
		 * @param ands conjunction clauses of the formula
		 * @param ors  disjunction clauses of the formula
		 *)
		and cross_product_of_conjunction ands ors =
			let cross cs1 cs2 =
				List.fold_left (fun acc1 c1 ->
					List.fold_left (fun acc2 c2 -> (And [c1; c2]) :: acc2) acc1 cs2
				) [] cs1
			in
			let merge_ands =
				List.fold_left (fun acc c ->
					match c with
					| And css -> (simplify_conjunction (And (List.append css ands))) :: acc
					| _       -> error 511
				) []
			in
			let rec iter cs =
				match cs with
				| []                           -> []
				| (Or cs) :: []                -> merge_ands cs
				| (Or cs1) :: (Or cs2) :: tail -> iter ((Or (cross cs1 cs2)) :: tail)
				| _                            -> error 512
			in
			dnf_of (Or (iter ors))

		(** convert a constraint to DNF **)
		and dnf_of c vars env =
			match c with
			| Eq e    -> dnf_of_equal e vars env
			| Ne e    -> dnf_of_not_equal e vars env
			| Not e   -> dnf_of_negation e vars env
			| Imply e -> dnf_of_implication e vars env
			| In e    -> dnf_of_membership e vars env
			| And e   -> dnf_of_conjunction e vars env
			| Or e    -> dnf_of_disjunction e vars env
			| _       -> c

		(** convert equality to DNF, and convert a left-nested reference to prevail ones **)
		and dnf_of_equal (r, v) vars env =
			if Variable.mem r vars then Eq (r, v)
			else dnf_of (constraints_of_nested r v vars env 1) vars env

		(** convert inequality to DNF, and convert a left-nested reference to prevail ones **)
		and dnf_of_not_equal (r, v) vars env =
			if Variable.mem r vars then
				let values =
					Array.fold_left (
						fun acc v1 ->
							match v1 with
							| Basic v2 -> if v2 = v then acc else v2 :: acc
							| _        -> error 513
					) [] (Variable.values_of r vars)
				in
				if values = [] then False
				else dnf_of (In (r, values)) vars env

			else
				dnf_of (constraints_of_nested r v vars env 2) vars env

		(** convert negation to DNF **)
		and dnf_of_negation c vars env =
			match c with
			| True        -> False
			| False       -> True
			| Eq e        -> dnf_of (Ne e) vars env
			| Ne e        -> dnf_of (Eq e) vars env
			| Not e       -> dnf_of e vars env
			| Imply (premise, conclusion) ->
				dnf_of (And [premise; (Not conclusion)]) vars env                (* -(p -> q) = p ^ -q *)
			| And cs      ->
				let cs1 = List.fold_left (fun acc c -> (Not c) :: acc) [] cs in  (* De Morgan's laws *)
				dnf_of (Or cs1) vars env
			| Or cs       ->
				let cs1 = List.fold_left (fun acc c -> (Not c) :: acc) [] cs in  (* De Morgan's laws *)
				dnf_of (And cs1) vars env
			| In (r, vec) ->
				if Variable.mem r vars then
					let cs = Array.fold_left (
							fun acc v ->
								match v with
								| Basic v1 ->
									if List.exists (fun v2 -> v1 = v2) vec then acc
									else (Eq (r, v1)) :: acc
								| _        -> error 514
						) [] (Variable.values_of r vars)
					in
					if cs = [] then False
					else dnf_of (Or cs) vars env
				else
					dnf_of (constraints_of_nested r (Vector vec) vars env 4) vars env

		(** convert implication to DNF **)
		and dnf_of_implication (premise, conclusion) vars env =
			dnf_of (Or [(Not premise); conclusion]) vars env

		(** convert membership constraint to DNF **)
		and dnf_of_membership (r, vec) vars env =
			if Variable.mem r vars then
				let cs = Array.fold_left (
						fun acc v ->
							match v with
							| Basic v1 ->
								if List.exists (fun v2 -> v1 = v2) vec then (Eq (r, v1)) :: acc
								else acc
							| _        -> error 515
					) [] (Variable.values_of r vars)
				in
				if cs = [] then False
				else dnf_of (Or cs) vars env
			else
				dnf_of (constraints_of_nested r (Vector vec) vars env 3) vars env

		(** convert conjunction to DNF, performs cross-products when it has disjunction clause **)
		and dnf_of_conjunction cs vars env =
			let rec iter clauses ands ors =
				if clauses = [] then (false, ands, ors)
				else
					match dnf_of (List.hd clauses) vars env with
					| And css -> iter (List.tl clauses) (List.append css ands) ors
					| False   -> (true, ands, ors)
					| True    -> iter (List.tl clauses) ands ors
					| Or css  -> iter (List.tl clauses) ands ((Or css) :: ors)
					| css     -> iter (List.tl clauses) (css :: ands) ors
			in
			let (all_false, ands, ors) = iter cs [] [] in
			if all_false then False
			else
				match ands, ors with
				| [], [] -> True
				| head :: [], [] -> head
				| [], head :: [] -> head
				| _, []          -> simplify_conjunction (And ands)
				| _, _           -> (cross_product_of_conjunction ands ors) vars env

		(** convert disjunction to DNF **)
		and dnf_of_disjunction cs vars env =
			let rec iter clauses acc =
				if clauses = [] then (false, acc)
				else
					match dnf_of (List.hd clauses) vars env with
					| Or cs -> iter (List.tl clauses) (List.append cs acc)
					| False -> iter (List.tl clauses) acc
					| True  -> (true, acc)
					| c     -> iter (List.tl clauses) (c :: acc)
			in
			let (all_true, cs1) = iter cs [] in
			if all_true then True
			else if (List.tl cs1) = [] then List.hd cs1
			else simplify_disjunction (Or cs1)

		(**
		 * substitute each left-hand side reference with a reference as
		 * specified in the parameters table
		 *)
		let substitute_parameter_of_reference (r: reference) params : reference =
			match r with
			| id :: tail ->
				if StrMap.mem id params then
					match StrMap.find id params with
					| Ref r1 -> r1 @++ tail
					| _      -> error 516 (* cannot replace left-hand side reference with a non-reference value *)
				else r
			| _ -> r

		(**
		 * substitute each right-hand side reference of basic value
		 * with a value as specified in the parameters table
		 *)
		let substitute_parameter_of_basic_value (bv: basic) params : basic =
			match bv with
			| Ref (id :: tail) ->
				if StrMap.mem id params then
					match StrMap.find id params with
					| Ref r1            -> Ref (r1 @++ tail)
					| v1 when tail = [] -> v1
					| _                 -> error 517
				else bv
			| _ -> bv
	
		(* substitute each parameter with a value as specified in the parameters table *)
		let rec substitute_parameters_of (c: _constraint) params =
			match c with
			| Eq (r, v) ->
				let r1 = substitute_parameter_of_reference r params in
				let v1 = substitute_parameter_of_basic_value v params in
				Eq (r1, v1)
			| Ne (r, v) ->
				let r1 = substitute_parameter_of_reference r params in
				let v1 = substitute_parameter_of_basic_value v params in
				Ne (r1, v1)
			| Not c          -> Not (substitute_parameters_of c params)
			| Imply (c1, c2) -> Imply (substitute_parameters_of c1 params, substitute_parameters_of c2 params)
			| And cs         -> And (List.fold_left (fun css c -> (substitute_parameters_of c params) :: css) [] cs)
			| Or cs          -> Or (List.fold_left (fun css c -> (substitute_parameters_of c params) :: css) [] cs)
			| In (r, v)      -> let r1 = substitute_parameter_of_reference r params in In (r1, v)
			| _              -> c

		(**
		 * Find the global constraints element in a flat-store. If exist, then convert
		 * and return a DNF of the global constraints
		 *)
		let global env fs vars =
			let r = ["global"] in
			if FlatStore.mem r fs then
				match FlatStore.find r fs with
				| Global g -> dnf_of g vars env
				| _        -> error 518
			else True

	end

(*******************************************************************
 * action
 *******************************************************************)

module Action =
	struct
		type t = reference * basic StrMap.t * cost * basic RefMap.t * basic RefMap.t;;

		let json_of_parameters ps =
			let s = StrMap.fold (fun id v s -> ",\"" ^ id ^ "\":" ^ (Sfdomainhelper.json_of_basic v) ^ s) ps "" in
			if s = "" then "" else (String.sub s 1 ((String.length s) - 1))

		let json_of (a: t) : string =
			let (name, ps, cost, pre, eff) = a in
			let string_of_map map =
				let s = RefMap.fold (fun r v acc -> acc ^ ",\"" ^ !^r ^ "\":" ^ (Sfdomainhelper.json_of_basic v)) map "" in
				if s = "" then "" else (String.sub s 1 ((String.length s) - 1))
			in
			sprintf "{\"name\":\"%s\",\"parameters\":{%s},\"cost\":%d,\"conditions\":{%s},\"effects\":{%s}}"
				!^name (json_of_parameters ps) cost (string_of_map pre) (string_of_map eff)

		let json_of_actions (actions: t list) : string =
			match actions with
			| [] -> "[]"
			| act :: [] -> "[" ^ (json_of act) ^ "]"
			| act :: acts -> "[" ^ (json_of act) ^ (List.fold_left (fun s a -> s ^ "," ^ (json_of a)) "" acts) ^ "]"

		(* convert a list of (identifier => type) to a list of maps of (identifier => value) *)
		let create_parameter_table params name typetable =
			let table1 = StrMap.add "this" [Ref (prefix name)] StrMap.empty in
			let table2 = List.fold_left (fun table (id, t) ->
					let values = Values.fold (fun v acc ->
							match v with
							| Basic bv -> bv :: acc
							| _ -> acc
						) (TypeTable.values_of t typetable) []
					in
					StrMap.add id values table
				) table1 params
			in
			StrMap.fold (fun id values acc1 ->
				List.fold_left (fun acc2 v ->
					if acc1 = [] then (StrMap.add id v StrMap.empty) :: acc2
					else List.fold_left (fun acc3 table -> (StrMap.add id v table) :: acc3) acc2 acc1
				) [] values
			) table2 []

		let equals_to_map = fun map c ->
			match c with
			| Eq (r, v) -> RefMap.add r v map
			| _         -> error 521

		(* ground an action - returns a list of grounded actions *)
		let ground_action_of (name, params, cost, pre, eff) env vars typetable dummy : t list =

			let map_effects effects params =
				List.fold_left (fun acc (r, bv) ->
					let r1 = Constraint.substitute_parameter_of_reference r params in
					let bv1 = Constraint.substitute_parameter_of_basic_value bv params in
					RefMap.add r1 bv1 acc
				) RefMap.empty effects
			in
			let param_tables = create_parameter_table params name typetable in
			List.fold_left (fun acc1 ps ->
				let eff1 = map_effects eff ps in
				let eff2 = if dummy then RefMap.add Variable.r_dummy (Boolean false) eff1 else eff1 in
				let pre1 = Constraint.substitute_parameters_of pre ps in
				let pre2 = if dummy then RefMap.add Variable.r_dummy (Boolean true) RefMap.empty else RefMap.empty in
				match Constraint.dnf_of pre1 vars env with
				| False -> acc1
				| Or cs -> List.fold_left (fun acc2 c ->
						let pre3 =
							match c with
							| Eq (r, v) -> RefMap.add r v pre2
							| And css   -> List.fold_left equals_to_map pre2 css
							| _         -> error 522
						in
						(name, ps, cost, pre3, eff2) :: acc2
					) acc1 cs
				| And css   ->
					let pre3 = List.fold_left equals_to_map pre2 css in
					(name, ps, cost, pre3, eff2) :: acc1
				| Eq (r, v) ->
					let pre3 = RefMap.add r v pre2 in
					(name, ps, cost, pre3, eff2) :: acc1
				| True      -> (name, ps, cost, pre2, eff2) :: acc1
				| _         -> error 523
			) [] param_tables

		let create_global_actions (global: _constraint) (acc: t list) : t list =
			let pre = RefMap.add Variable.r_dummy (Boolean false) RefMap.empty in
			let eff = RefMap.add Variable.r_dummy (Boolean true) RefMap.empty in
			let params = StrMap.empty in
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
			| _     -> error 524

		(* ground a set of actions - returns a list of grounded actions *)
		let ground env vars arr_vars typetable global : t list =
			let add_dummy = not (global = True) in
			let actions : t list = create_global_actions global [] in
			Values.fold (
				fun v gactions ->
					match v with
					| Action a -> List.append (ground_action_of a env vars typetable add_dummy) gactions
					| _        -> gactions
			) (TypeTable.values_of t_action typetable) actions

		(* type t = reference * basic StrMap.t * cost * _constraint * effect list;; *)

		let fdr_of (buf: Buffer.t) ((name, params, cost, pre, eff): t) (vars: Variable.t RefMap.t) counter : unit =
			Buffer.add_string buf "\nbegin_operator\nop_";
			(* name *)
			Buffer.add_string buf (string_of_int !counter);
			Buffer.add_char buf ' ';
			Buffer.add_string buf !^(name);
			Buffer.add_string buf " {";
			Buffer.add_string buf (json_of_parameters params);
			Buffer.add_string buf "}\n";
			(* prevail *)
			let prevail = Buffer.create 50 in
			let n = ref 0 in
			RefMap.iter (fun r v ->
				if not (RefMap.mem r eff) then
				(
					let var = Variable.find r vars in
					Buffer.add_string prevail (string_of_int var.index);
					Buffer.add_char prevail ' ';
					Buffer.add_string prevail (string_of_int (Variable.index_of var (Basic v)));
					Buffer.add_char prevail '\n';
					n := !n + 1;
				)
			) pre;
			Buffer.add_string buf (string_of_int !n);
			Buffer.add_char buf '\n';
			Buffer.add_string buf (Buffer.contents prevail);
			(* prepost *)
			Buffer.add_string buf (string_of_int (RefMap.cardinal eff));
			Buffer.add_char buf '\n';
			RefMap.iter (fun r v ->
				let var = Variable.find r vars in
				Buffer.add_string buf "0 ";
				Buffer.add_string buf (string_of_int var.index);
				Buffer.add_char buf ' ';
				if RefMap.mem r pre then
					let pre_v = RefMap.find r pre in
					Buffer.add_string buf (string_of_int (Variable.index_of var (Basic pre_v)));
				else
					Buffer.add_string buf "-1";
				Buffer.add_char buf ' ';
				Buffer.add_string buf (string_of_int (Variable.index_of var (Basic v)));
				Buffer.add_char buf '\n';
			) eff;
			(* cost *)
			Buffer.add_string buf (string_of_int cost);
			Buffer.add_string buf "\nend_operator";
			counter := !counter + 1;;

		let fdr_of_actions (buf: Buffer.t) (actions: t list) (vars: Variable.t RefMap.t) : unit =
			let counter = ref 0 in
			let buf_actions = Buffer.create 50 in
			List.iter (fun a -> fdr_of buf_actions a vars counter) actions;
			Buffer.add_char buf '\n';
			Buffer.add_string buf (string_of_int !counter);
			Buffer.add_string buf (Buffer.contents buf_actions);;

		let fdr_of_mutex (buf: Buffer.t) : unit =
			Buffer.add_string buf "\n0"

	end

(*******************************************************************
 * interface functions for translating SFP to FDR
 *******************************************************************)

let fdr_version = "3";;
let fdr_metric = "1";;
let fdr_header buf =
	Buffer.add_string buf "begin_version\n";
	Buffer.add_string buf fdr_version;
	Buffer.add_string buf "\nend_version\nbegin_metric\n";
	Buffer.add_string buf fdr_metric;
	Buffer.add_string buf "\nend_metric";;

(** step 1: generate flat-stores of current and desired specifications **)
let normalise store_0 store_g = (FlatStore.make store_0, FlatStore.make store_g)

(** step 2: translate **)
let translate env_0 fs_0 env_g fs_g typetable : string =
	let (map_vars, arr_vars) = Variable.make env_0 fs_0 env_g fs_g typetable in
	let global = Constraint.global env_g fs_g map_vars in
	let actions = Action.ground env_g map_vars arr_vars typetable global in
	let buffer = Buffer.create (100 + (40 * (Array.length arr_vars) * 2)) in
	fdr_header buffer;
	Variable.fdr_of_variables buffer arr_vars;
	Variable.fdr_of_mutex buffer arr_vars;
	Variable.fdr_of_init buffer arr_vars;
	Variable.fdr_of_goal buffer arr_vars (not (global = True));
	Action.fdr_of_actions buffer actions map_vars;
	Action.fdr_of_mutex buffer;
	Buffer.contents buffer

let compile ast = (TEnv.make (Sftype.sfpSpecification ast), Sfvaluation.sfpSpecification ast)

let fdr ast_0 ast_g =
	let (env_0, store_0) = compile ast_0 in
	let (env_g, store_g) = compile ast_g in
	let (fs_0, fs_g) = normalise store_0 store_g in
	let typetable = TypeTable.make env_0 fs_0 env_g fs_g in
	let fdr = translate env_0 fs_0 env_g fs_g typetable in
	fdr


