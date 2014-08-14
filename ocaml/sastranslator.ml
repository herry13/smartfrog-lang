open Printf
open Common
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

(*******************************************************************
 * flat-store
 *******************************************************************)

module TEnv =
	struct
		type t = Sfsyntax._type MapRef.t

		let empty = MapRef.empty;;
		let mem = MapRef.mem;;
		let add = MapRef.add;;
		let find = MapRef.find;;
		let fold = MapRef.fold;;

		let make e : t = List.fold_left (fun acc (r, t) -> add r t acc) empty e

		let type_of e r = if mem r e then find r e else Sfsyntax.TUndefined

	end

module FlatStore =
	struct
		type t = value MapRef.t

		let empty = MapRef.empty;;
		let mem = MapRef.mem;;
		let add = MapRef.add;;
		let find = MapRef.find;;
		let fold = MapRef.fold;;

		let static_object = Store []

		(**
		 * Convert a store to a flat_store
		 *
		 * @param s store to be flatten
		 * @return a flat_store
		 *)
		let normalise (s: store) : t =
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

		type t_arr = value array;;
		type t_list = value list;;

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
 * variables
 *******************************************************************)

let make_variables (env_0: TEnv.t) (fs_0: FlatStore.t) (env_g: TEnv.t) (fs_g: FlatStore.t) typetable : Variable.ts =
	let type_of_var r =
		match (TEnv.type_of env_0 r), (TEnv.type_of env_g r) with
		| t1, t2 when t1 = t2 -> t1
		| _, _                -> error 504  (* incompatible type between init & goal *)
	in
	let map0 = MapRef.add Variable.r_dummy Variable.dummy MapRef.empty in
	let arr0 = [Variable.dummy] in
	let (map1, total, arr1) = FlatStore.fold (
			fun r v (map, i, arr) ->
				if MapRef.mem r map then error 505;
				match type_of_var r with
				| Sfsyntax.TBasic Sfsyntax.TAction
				| Sfsyntax.TBasic Sfsyntax.TGlobal -> (map, i, arr)
				| Sfsyntax.TBasic Sfsyntax.TObject
				| Sfsyntax.TBasic Sfsyntax.TSchema (_, _) ->
					let v = FlatStore.static_object in
					let var = Variable.make r i [|v|] v v in
					let map1 = MapRef.add r var map in
					let arr1 = var :: arr in
					(map1, i+1, arr1)
				| t ->
					let values = Array.of_list (Values.elements (TypeTable.values_of t typetable)) in
					let init = FlatStore.find r fs_0 in
					let goal = FlatStore.find r fs_g in
					let var = Variable.make r i values init goal in
					let map1 = MapRef.add r var map in
					let arr1 = var :: arr in
					(map1, i+1, arr1)
		) fs_0 (map0, 1, arr0)
	in
	let vars = Variable.make_ts map1 (Array.of_list arr1) in
	Variable.sort vars;
	vars

(*******************************************************************
 * constraints
 *******************************************************************)

module Constraint =
	struct
		let eval s = false (* TODO *)

		let string_of = Sfdomainhelper.json_of_constraint

		(* mode: {1 => Eq, 2 => Ne, 3 => In, 4 => NotIn } *)
		let rec constraints_of_nested r v vars env mode =
			let rec prevail_of rx =
				if rx = [] then error 507
				else if Variable.mem rx vars then rx
				else prevail_of (prefix rx)
			in
			let rec iter r cs =
				let prevail = prevail_of r in
				if prevail = r then
					match mode, v with
					| 1, _          -> (Eq (r, v)) :: cs
					| 2, _          -> (Ne (r, v)) :: cs
					| 3, Vector vec -> (In (r, vec)) :: cs
					| 4, Vector vec -> (Not (In (r, vec))) :: cs
					| _             -> error 508
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
							| _              -> error 509
					) cs1 (Variable.values_of prevail vars)
			in
			And (iter r [])

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
					if List.exists (fun c2 ->
							(* return false if formula 'c1' negates 'c2' (or vice versa), otherwise true *)
							match c1, c2 with
							| Eq (r1, v1), Eq (r2, v2) -> if r1 = r2 then not (v1 = v2) else false
							| Eq (r1, v1), Ne (r2, v2) -> if r1 = r2 then v1 = v2 else false
							| Ne (r1, v1), Eq (r2, v2) -> if r1 = r2 then v1 = v2 else false
							| _                        -> false
						) tail then False
					else if List.mem c1 tail then iter tail acc
					else iter tail (c1 :: acc)
			in
			match c with
			| And clauses -> iter clauses []
			| _           -> error 510

		(**
		 * simplify a disjunction formula
		 * - remove duplications
		 *)
		and simplify_disjunction c =
			let rec iter clauses acc =
				match clauses with
				| [] -> Or acc
				| c1 :: tail ->
					if List.mem c1 tail then iter tail acc
					else iter tail (c1 :: acc)
			in
			match c with
			| Or clauses -> iter clauses []
			| _          -> error 511

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
					| _       -> error 512
				) []
			in
			let rec iter cs =
				match cs with
				| []                           -> []
				| (Or cs) :: []                -> merge_ands cs
				| (Or cs1) :: (Or cs2) :: tail -> iter ((Or (cross cs1 cs2)) :: tail)
				| _                            -> error 513
			in
			dnf_of (Or (iter ors))

		(** convert a constraint to DNF **)
		and dnf_of c (vars: Variable.ts) env =
			match c with
			| Eq (r, v)    -> dnf_of_equal r v vars env
			| Ne (r, v)    -> dnf_of_not_equal r v vars env
			| Not _   -> dnf_of_negation c vars env
			| Imply (c1, c2) -> dnf_of_implication c1 c2 vars env
			| In (r, vec)    -> dnf_of_membership r vec vars env
			| And cs   -> dnf_of_conjunction cs vars env
			| Or cs    -> dnf_of_disjunction cs vars env
			| _       -> c

		(** convert equality to DNF, and convert a left-nested reference to prevail ones **)
		and dnf_of_equal r v vars env =
			if Variable.mem r vars then Eq (r, v)
			else dnf_of (constraints_of_nested r v vars env 1) vars env

		(** convert inequality to DNF, and convert a left-nested reference to prevail ones **)
		and dnf_of_not_equal r v vars env =
			if Variable.mem r vars then
				let values =
					Array.fold_left (
						fun acc v1 ->
							match v1 with
							| Basic v2 -> if v2 = v then acc else v2 :: acc
							| _        -> error 514
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
			| Eq (r, v)        -> dnf_of (Ne (r, v)) vars env
			| Ne (r, v)        -> dnf_of (Eq (r, v)) vars env
			| Not _       -> dnf_of c vars env
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
									if List.mem v1 vec then acc
									else (Eq (r, v1)) :: acc
								| _        -> error 515
						) [] (Variable.values_of r vars)
					in
					if cs = [] then False
					else dnf_of (Or cs) vars env
				else
					dnf_of (constraints_of_nested r (Vector vec) vars env 4) vars env

		(** convert implication to DNF **)
		and dnf_of_implication premise conclusion vars env =
			dnf_of (Or [(Not premise); conclusion]) vars env

		(** convert membership constraint to DNF **)
		and dnf_of_membership r vec vars env =
			if Variable.mem r vars then
				let cs = Array.fold_left (
						fun acc v ->
							match v with
							| Basic v1 ->
								if List.mem v1 vec then (Eq (r, v1)) :: acc
								else acc
							| _        -> error 516
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
		let substitute_parameter_of_basic_value (bv: basic) params : basic =
			match bv with
			| Ref (id :: tail) ->
				if MapStr.mem id params then
					match MapStr.find id params with
					| Ref r1            -> Ref (r1 @++ tail)
					| v1 when tail = [] -> v1
					| _                 -> error 518
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

		(************************************************************************
		 * functions to compile simple membership and equality
		 ************************************************************************)
		type constraints_variables = { cons: _constraint list; implies: _constraint list; vars: Variable.ts }

		let compile_simple_global_membership negation r (vec: vector) cons_vars env =
			let rec prevail_of rx =
				if rx = [] then error 507
				else if Variable.mem rx cons_vars.vars then rx
				else prevail_of (prefix rx)
			in
			let prevail = prevail_of r in
			if prevail = r then
				{	cons    = cons_vars.cons;
					implies = cons_vars.implies;
					vars    = if negation then Variable.remove_values_from r vec cons_vars.vars
					          else Variable.intersection_with_values r vec cons_vars.vars
				}
			else
				let vars1 = Variable.remove_value_from prevail (Basic Null) cons_vars.vars in
				let rs = r @-- prevail in
				Array.fold_left (fun acc v1 ->
						match v1 with
						| Basic (Ref r1) ->
							let r2 = r1 @++ rs in
							let c = (* TODO: convert (Eq => In) to (Eq => Eq)...(Eq => Eq) *)
								if negation then Imply (Eq (prevail, Ref r1), Not (In (r2, vec)))
								else Imply (Eq (prevail, Ref r1), In (r2, vec))
							in
							if Variable.mem r2 acc.vars then
								{ cons = acc.cons; implies = c :: acc.implies; vars = acc.vars }
							else
								{ cons = c :: acc.cons; implies = acc.implies; vars = acc.vars }
						| _              -> error 508
					)
					{ cons = cons_vars.cons; implies = cons_vars.implies; vars = vars1 }
					(Variable.values_of prevail vars1)

		let compile_simple_global_equality negation r v cons_vars env =
			let rec prevail_of rx =
				if rx = [] then error 509
				else if Variable.mem rx cons_vars.vars then rx
				else prevail_of (prefix rx)
			in
			let prevail = prevail_of r in
			if prevail = r then
				{	cons    = cons_vars.cons;
					implies = cons_vars.implies;
					vars    = if negation then Variable.remove_value_from r (Basic v) cons_vars.vars
					          else Variable.intersection_with_value r (Basic v) cons_vars.vars
				}
			else 
				let vars1 = Variable.remove_value_from prevail (Basic Null) cons_vars.vars in
				let rs = r @-- prevail in
				Array.fold_left (fun acc v1 ->
						match v1 with
						| Basic (Ref r1) ->
							let r2 = r1 @++ rs in
							let c = (* TODO: convert (Eq => Ne) to (Eq => Eq)...(Eq => Eq) *)
								if negation then Imply (Eq (prevail, Ref r1), Ne (r2, v))
								else Imply (Eq (prevail, Ref r1), Eq (r2, v))
							in
							if Variable.mem r2 acc.vars then
								{ cons = acc.cons; implies = c :: acc.implies; vars = acc.vars }
							else
								{ cons = c :: acc.cons; implies = acc.implies; vars = acc.vars }
						| _ -> error 510
					)
					{ cons = cons_vars.cons; implies = cons_vars.implies; vars = vars1 }
					(Variable.values_of prevail vars1)

		let compile_simple_global global vars env =
			match global with
			| And cs ->
				let result1 = List.fold_left (fun acc c ->
						match c with
						| Eq (r, v)         -> compile_simple_global_equality false r v acc env
						| Not (Eq (r, v))   -> compile_simple_global_equality true r v acc env
						| Ne (r, v)         -> compile_simple_global_equality true r v acc env
						| Not (Ne (r, v))   -> compile_simple_global_equality false r v acc env
						| In (r, vec)       -> compile_simple_global_membership false r vec acc env
						| Not (In (r, vec)) -> compile_simple_global_membership true r vec acc env
						| _                 -> { cons = c :: acc.cons; implies = acc.implies; vars = acc.vars }
					) { cons = []; implies = []; vars = vars } cs
				in
				(And result1.cons, result1.implies, result1.vars)
			| _  -> (global, [], vars)

		(**
		 * Find the global constraints element in a flat-store. If exist, then convert
		 * and return a DNF of the global constraints
		 *)
		let global (env: Sfsyntax._type MapRef.t) (fs: value MapRef.t) (vars: Variable.ts) =
			let r = ["global"] in
			if FlatStore.mem r fs then
				match FlatStore.find r fs with
				| Global g ->
					let (global1, implies1, vars1) = compile_simple_global g vars env in
					let global_dnf = dnf_of global1 vars1 env in
					(global_dnf, implies1, vars1)
				| _        -> error 519
			else (True, [], vars)

	end

(*******************************************************************
 * action
 *******************************************************************)

module Action =
	struct
		type t = reference * basic MapStr.t * cost * basic MapRef.t * basic MapRef.t;;

		let json_of_parameters (ps: basic MapStr.t) : string =
			let s = MapStr.fold (fun id v s -> ",\"" ^ id ^ "\":" ^ (Sfdomainhelper.json_of_basic v) ^ s) ps "" in
			if s = "" then "" else (String.sub s 1 ((String.length s) - 1));;

		let json_of_preconditions (pre: basic MapRef.t) : string =
			let buf = Buffer.create 42 in
			MapRef.iter (fun r v ->
				Buffer.add_string buf ",\"";
				Buffer.add_string buf !^r;
				Buffer.add_string buf "\":";
				Buffer.add_string buf (Sfdomainhelper.json_of_basic v)
			) pre;
			let s = Buffer.contents buf in
			if s = "" then "{}" else "{" ^ (String.sub s 1 ((String.length s) -1)) ^ "}";;

		let json_of_effects = json_of_preconditions;;

		let json_of (a: t) : string =
			let (name, ps, cost, pre, eff) = a in
			sprintf "{\"name\":\"%s\",\"parameters\":{%s},\"cost\":%d,\"conditions\":{%s},\"effects\":{%s}}"
				!^name (json_of_parameters ps) cost (json_of_preconditions pre) (json_of_effects eff);;

		let json_of_actions (actions: t list) : string =
			match actions with
			| [] -> "[]"
			| act :: [] -> "[" ^ (json_of act) ^ "]"
			| act :: acts -> "[" ^ (json_of act) ^ (List.fold_left (fun s a -> s ^ "," ^ (json_of a)) "" acts) ^ "]"

		(* convert a list of (identifier => type) to a list of maps of (identifier => value) *)
		let create_parameter_table params name typetable =
			let table1 = MapStr.add "this" [Ref (prefix name)] MapStr.empty in
			let table2 = List.fold_left (fun table (id, t) ->
					let values = Values.fold (fun v acc ->
							match v with
							| Basic bv -> bv :: acc
							| _ -> acc
						) (TypeTable.values_of t typetable) []
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
		let ground_action_of (name, params, cost, pre, eff) env vars typetable dummy (g_implies : _constraint list) : t list =
			let param_tables = create_parameter_table params name typetable in
			List.fold_left (fun acc1 ps ->
				let eff1 = List.fold_left (fun acc (r, bv) ->
						let r1 = Constraint.substitute_parameter_of_reference r ps in
						let bv1 = Constraint.substitute_parameter_of_basic_value bv ps in
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
		let ground_actions env vars typetable global (g_implies : _constraint list) : t list =
			print_endline (Constraint.string_of (And g_implies));
			let add_dummy = not (global = True) in
			let actions : t list = create_global_actions global [] in
			Values.fold (
				fun v gactions ->
					match v with
					| Action a -> List.append (ground_action_of a env vars typetable add_dummy g_implies) gactions
					| _        -> gactions
			) (TypeTable.values_of t_action typetable) actions

	end

(*******************************************************************
 * interface functions for translating SFP to FDR
 *******************************************************************)

module FDR = struct
	let version = "3";;
	let metric = "1";;
	let of_header (buf: Buffer.t) : unit =
		Buffer.add_string buf "begin_version\n";
		Buffer.add_string buf version;
		Buffer.add_string buf "\nend_version\nbegin_metric\n";
		Buffer.add_string buf metric;
		Buffer.add_string buf "\nend_metric";;

	let of_variables (buf: Buffer.t) (vars: Variable.ts) : unit =
		Buffer.add_char buf '\n';
		Buffer.add_string buf (string_of_int (Variable.total vars));
		Variable.iter (fun var ->
			Buffer.add_string buf "\nbegin_variable\nvar";
			Buffer.add_string buf (string_of_int (Variable.index var));
			Buffer.add_char buf '_';
			Buffer.add_string buf !^(Variable.name var);
			Buffer.add_string buf "\n-1\n";
			Buffer.add_string buf (string_of_int (Variable.size var));
			Buffer.add_char buf '\n';
			Variable.iteri_values (fun i v ->
				Buffer.add_string buf (Sfdomainhelper.json_of_value v);
				Buffer.add_char buf '\n';
			) var;
			Buffer.add_string buf "end_variable"
		) vars;;

	let of_init (buf: Buffer.t) (vars: Variable.ts) : unit =
		Buffer.add_string buf "\nbegin_state";
		Variable.iter (fun var ->
			let i = Variable.index_of_value (Variable.init var) var in
			if i < 0 then error 601; (* initial state is not satisfying the global constraint *)
			Buffer.add_char buf '\n';
			Buffer.add_string buf (string_of_int i)
		) vars;
		Buffer.add_string buf "\nend_state";;

	let of_goal (buf: Buffer.t) (vars: Variable.ts) use_dummy : unit =
		let tmp = Buffer.create 50 in
		let counter = ref 0 in
		Variable.iter (fun var ->
			(**
			 * set the variable's goal, when:
			 * - it has more than one value
			 * - if it is a dummy variable, then the global constraint must be exist
			 *)
			let i = Variable.index_of_value (Variable.goal var) var in
			if i < 0 then error 602; (* goal state is not satisfying the global constraint *)
			if (Variable.size var) > 1 && ((Variable.index var) > 0 || use_dummy) then
			(
				Buffer.add_char tmp '\n';
				Buffer.add_string tmp (string_of_int (Variable.index var));
				Buffer.add_char tmp ' ';
				Buffer.add_string tmp (string_of_int i);
				counter := !counter + 1;
			)
		) vars;
		Buffer.add_string buf "\nbegin_goal";
		Buffer.add_char buf '\n';
		Buffer.add_string buf (string_of_int !counter);
		Buffer.add_string buf (Buffer.contents tmp);
		Buffer.add_string buf "\nend_goal";;

	(**
	 * Generate the FDR mutex - TODO
	 *)
	let of_mutex (buf: Buffer.t) : unit =
		Buffer.add_string buf "\n0";;

	(**
	 * Generate the FDR of a given action. If there is an assignment whose value is not
	 * in the variable's domain, then the action is treated as "invalid".
	 *)
	let of_action (buffer: Buffer.t) ((name, params, cost, pre, eff): Action.t) (vars: Variable.ts) counter : unit =
		let valid_operator = ref true in
		let buf = Buffer.create 50 in
		Buffer.add_string buf "\nbegin_operator\nop_";
		(* name *)
		Buffer.add_string buf (string_of_int !counter);
		Buffer.add_char buf ' ';
		Buffer.add_string buf !^(name);
		Buffer.add_string buf " {";
		Buffer.add_string buf (Action.json_of_parameters params);
		Buffer.add_string buf "}\n";
		(* prevail *)
		let prevail = Buffer.create 50 in
		let n = ref 0 in
		MapRef.iter (fun r v ->
			if not (MapRef.mem r eff) then
			(
				let var = Variable.find r vars in
				Buffer.add_string prevail (string_of_int (Variable.index var));
				Buffer.add_char prevail ' ';
				let i = Variable.index_of_value (Basic v) var in
				if i < 0 then valid_operator := false;
				Buffer.add_string prevail (string_of_int i);
				Buffer.add_char prevail '\n';
				n := !n + 1;
			)
		) pre;
		Buffer.add_string buf (string_of_int !n);
		Buffer.add_char buf '\n';
		Buffer.add_string buf (Buffer.contents prevail);
		(* prepost *)
		Buffer.add_string buf (string_of_int (MapRef.cardinal eff));
		Buffer.add_char buf '\n';
		MapRef.iter (fun r v ->
			let var = Variable.find r vars in
			Buffer.add_string buf "0 ";
			Buffer.add_string buf (string_of_int (Variable.index var));
			Buffer.add_char buf ' ';
			if MapRef.mem r pre then
				let pre_v = MapRef.find r pre in
				let i = Variable.index_of_value (Basic pre_v) var in
				if i < 0 then valid_operator := false;
				Buffer.add_string buf (string_of_int i);
			else
				Buffer.add_string buf "-1";
			Buffer.add_char buf ' ';
			let j = Variable.index_of_value (Basic v) var in
			if j < 0 then valid_operator := false;
			Buffer.add_string buf (string_of_int j);
			Buffer.add_char buf '\n';
		) eff;
		(* check operator validity *)
		if !valid_operator then (
			(* cost *)
			Buffer.add_string buf (string_of_int cost);
			Buffer.add_string buf "\nend_operator";
			Buffer.add_string buffer (Buffer.contents buf);
			counter := !counter + 1
		)
		else prerr_endline ("Warning: operator " ^ !^name ^ " is invalid")

	(* generate FDR of a set of actions *)
	let of_actions (buf: Buffer.t) (actions: Action.t list) (vars: Variable.ts) : unit =
		let counter = ref 0 in
		let buf_actions = Buffer.create 50 in
		List.iter (fun a -> of_action buf_actions a vars counter) actions;
		Buffer.add_char buf '\n';
		Buffer.add_string buf (string_of_int !counter);
		Buffer.add_string buf (Buffer.contents buf_actions);;

	(* generate FDR axioms *)
	let of_axioms (buf: Buffer.t) : unit =
		Buffer.add_string buf "\n0";;

	let of_sfp (vars: Variable.ts) (actions: Action.t list) (global: _constraint) (g_implies: _constraint list) : string =
		let buffer = Buffer.create (100 + (40 * (Variable.total vars) * 2)) in
		of_header buffer;
		of_variables buffer vars;
		of_mutex buffer;
		of_init buffer vars;
		of_goal buffer vars (not (global = True));
		of_actions buffer actions vars;
		of_axioms buffer;
		Buffer.contents buffer
end

(** step 1: generate flat-stores of current and desired specifications **)
let normalise store_0 store_g = (FlatStore.make store_0, FlatStore.make store_g)

(** step 2: translate **)
let translate (env_0: TEnv.t) (fs_0: FlatStore.t) (env_g: TEnv.t) (fs_g: FlatStore.t) typetable : string =
	let vars = make_variables env_0 fs_0 env_g fs_g typetable in
	let (global, g_implies, vars1) = Constraint.global env_g fs_g vars in
	let actions = Action.ground_actions env_g vars1 typetable global g_implies in
	FDR.of_sfp vars1 actions global g_implies

let compile (ast: Sfsyntax.sfp) : TEnv.t * store = (TEnv.make (Sftype.sfpSpecification ast), Sfvaluation.sfpSpecification ast)

let fdr (ast_0: Sfsyntax.sfp) (ast_g: Sfsyntax.sfp) : string =
	let (env_0, store_0) = compile ast_0 in
	let (env_g, store_g) = compile ast_g in
	let (fs_0, fs_g) = normalise store_0 store_g in
	let typetable = TypeTable.make env_0 fs_0 env_g fs_g in
	let fdr = translate env_0 fs_0 env_g fs_g typetable in
	fdr


