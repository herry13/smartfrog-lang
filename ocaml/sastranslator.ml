open Sfdomain

(*******************************************************************
 * Modules and functions to convert SFP to FD-SAS.
 * There are three modules:
 * - FlatStore : a map which is the flat version of store domain
 * - Values    : a set of values (of particular type)
 * - TypeTable : a map of type -> values
 *******************************************************************)

(*******************************************************************
 * flat-store
 *******************************************************************)

module TEnv =
	struct
		module Map = Map.Make
			( struct
				type t = reference
				let compare = Pervasives.compare
			end )

		let make e = List.fold_left (fun acc (r, t) -> Map.add r t acc) Map.empty e

		let type_of e r = if Map.mem r e then Map.find r e else Sfsyntax.TUndefined

	end

module FlatStore =
	struct
		module Map = Map.Make
			( struct
				type t = reference
				let compare = Pervasives.compare
			end )

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
						| Store child -> visit child r (Map.add r (Store []) fs)
						| _           -> Map.add r v fs
					in
					visit tail ns fss
			in
			visit s [] Map.empty

		(**
		 * convert a flat-store to string
		 *)
		let string_of fs =
			Map.fold (
				fun r v acc -> acc ^ !^r ^ ": " ^ (Sfdomainhelper.json_of_value v) ^ "\n"
			) fs ""

		let find = Map.find

		let fold = Map.fold

		let make = normalise

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
	end

(*******************************************************************
 * type-table
 *******************************************************************)

let t_bool = Sfsyntax.TBasic Sfsyntax.TBool
let t_num = Sfsyntax.TBasic Sfsyntax.TNum
let t_str = Sfsyntax.TBasic Sfsyntax.TStr
let t_obj = Sfsyntax.TBasic Sfsyntax.TObject
let t_action = Sfsyntax.TBasic Sfsyntax.TAction
let t_global = Sfsyntax.TBasic Sfsyntax.TGlobal

module TypeTable =
	struct
		module Map = Map.Make
			( struct
				type t = Sfsyntax._type
				let compare = Pervasives.compare
			end )

		let values_of t table = if Map.mem t table then Map.find t table else Values.empty

		let add_value t v table = Map.add t (Values.add v (values_of t table)) table

		let string_of table = Map.fold (
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
			let table00 = add_store_values env_0 fs_0 Map.empty in
			let table01 = add_action_values env_0 table00 in
			let table10 = add_store_values env_g fs_g table01 in
			add_action_values env_g table10

	end

(*******************************************************************
 * FDR variables
 *******************************************************************)

module Variable =
	(* variable := name * index * values * init * goal *)
	struct
		module Map = Map.Make
			( struct
				type t = reference
				let compare = Pervasives.compare
			end )

		let mem = Map.mem

		let find r = Map.find r

		let values_of r map =
			if mem r map then
				let (_, _, values, _, _) = find r map in
				values
			else [| |]

		let string_of_values =
			Array.fold_left (fun acc v -> acc ^ (Sfdomainhelper.json_of_value v) ^ ";") ""

		let string_of_variable (name, index, values, init, goal) =
			!^name ^ "|" ^ string_of_int(index) ^ "|" ^ (string_of_values values) ^ "|" ^
			(Sfdomainhelper.json_of_value init) ^ "|" ^ (Sfdomainhelper.json_of_value goal)

		let string_of_array arr =
			Array.fold_left (fun acc var -> (string_of_variable var) ^ "\n" ^ acc) "" arr

		let make env_0 fs_0 env_g fs_g typetable =
			let type_of_var r =
				match (TEnv.type_of env_0 r), (TEnv.type_of env_g r) with
				| t1, t2 when t1 = t2 -> t1
				| _, _                -> error 504  (* incompatible type between init & goal *)
			in
			let (map, total, l) = FlatStore.fold (
					fun r v (map, i, arr) ->
						if Map.mem r map then error 505;
						match type_of_var r with
						| Sfsyntax.TBasic Sfsyntax.TAction
						| Sfsyntax.TBasic Sfsyntax.TGlobal -> (map, i, arr)
						| t ->
							let values = Array.of_list (Values.elements (TypeTable.values_of t typetable)) in
							let init = FlatStore.find r fs_0 in
							let goal = FlatStore.find r fs_g in
							let var = (r, i, values, init, goal) in
							let map1 = Map.add r var map in
							let arr1 = var :: arr in
							(map1, i+1, arr1)
				) fs_0 (Map.empty, 0, [])
			in
			let arr = Array.of_list l in
			Array.fast_sort (fun (_, i1, _, _, _) (_, i2, _, _, _) -> i1 - i2) arr;
			(map, arr)

	end

(*******************************************************************
 * constraints
 *******************************************************************)

module Constraint =
	struct
		let eval s = false (* TODO *)

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
								let conclusion = And (iter (rp @++ rs1) []) in
								let premise = Eq (prevail, Ref rp) in
								(Imply (premise, conclusion)) :: acc
							| Basic Null     -> acc
							| _              -> error 508
					) cs1 (Variable.values_of prevail vars)
			in
			And (iter r [])

		(* return false if formula 'c1' negates 'c2', otherwise true *)
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
							| _       -> error 513
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
							| _ -> error 515
					) [] (Variable.values_of r vars)
				in
				if cs = [] then False
				else dnf_of (Or cs) vars env
			else
				dnf_of (constraints_of_nested r (Vector vec) vars env 3) vars env

		(** convert conjunction to DNF, performs cross-products when it has disjunction clause **)
		and dnf_of_conjunction cs vars env =
			let all_false = ref false in
			let (ands, ors) = 
				List.fold_left (fun (ands, ors) c ->
					match dnf_of c vars env with
					| And css -> (List.append css ands, ors)
					| True    -> (ands, ors)
					| False   -> all_false := true; (ands, ors)
					| Or css  -> (ands, (Or css) :: ors)
					| css     -> (css :: ands, ors)
				) ([], []) cs
			in
			match ands, ors with
			| _, _           when !all_false -> False
			| [], []                         -> True
			| head :: [], []                 -> head
			| [], head :: []                 -> head
			| _, []                          -> simplify_conjunction (And ands)
			| _, _                           -> (cross_product_of_conjunction ands ors) vars env

		(** convert disjunction to DNF **)
		and dnf_of_disjunction cs vars env = 
			let all_true = ref false in
			let cs1 =
				List.fold_left (fun acc c ->
					match dnf_of c vars env with
					| Or cs -> List.append cs acc
					| True  -> all_true := true; acc
					| False -> acc
					| c     -> c :: acc
				) [] cs
			in
			if cs1 = [] || !all_true then True
			else if (List.tl cs1) = [] then List.hd cs1
			else simplify_disjunction (Or cs1)

		(** return a DNF of global constraints *)
		let global fs vars env =
			match FlatStore.find ["global"] fs with
			| Global g -> dnf_of g vars env
			| _        -> error 516

	end

(*******************************************************************
 * interface functions for translating SFP to FDR
 *******************************************************************)

(** step 1: generate flat-stores of current and desired specifications **)
let normalise store_0 store_g = (FlatStore.make store_0, FlatStore.make store_g)

(** step 2: translate **)
let translate env_0 fs_0 env_g fs_g typetable =
	let (map_vars, arr_vars) = Variable.make env_0 fs_0 env_g fs_g typetable in
	let global = Constraint.global fs_g map_vars env_g in
	"--- variables ---\n" ^ (Variable.string_of_array arr_vars) ^
	"--- global ---\n" ^ (Sfdomainhelper.json_of_constraint global)

let compile ast = (TEnv.make (Sftype.sfpSpecification ast), Sfvaluation.sfpSpecification ast)

let fdr ast_0 ast_g =
	let (env_0, store_0) = compile ast_0 in
	let (env_g, store_g) = compile ast_g in
	let (fs_0, fs_g) = normalise store_0 store_g in
	let typetable = TypeTable.make env_0 fs_0 env_g fs_g in
	let fdr = translate env_0 fs_0 env_g fs_g typetable in
	(* "=== flat-store current ===\n" ^ (string_of_flat_store fs_0) ^
	"=== flat-store desired ===\n" ^ (string_of_flat_store fs_g) ^ *)
	"=== type table ===\n" ^ (TypeTable.string_of typetable) ^
	"=== finite domain representation ===\n" ^ fdr


module Action =
	struct
		let applicable s = false (* TODO *)
		let apply s = s (* TODO *)
	end
