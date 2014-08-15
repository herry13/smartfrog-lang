open Common
open Sfdomain

let eval (s: store) (c: _constraint) : bool = false (* TODO *)

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
	| Eq (r, v)      -> dnf_of_equal r v vars env
	| Ne (r, v)      -> dnf_of_not_equal r v vars env
	| Not c1         -> dnf_of_negation c1 vars env
	| Imply (c1, c2) -> dnf_of_implication c1 c2 vars env
	| In (r, vec)    -> dnf_of_membership r vec vars env
	| And cs         -> dnf_of_conjunction cs vars env
	| Or cs          -> dnf_of_disjunction cs vars env
	| _              -> c

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
	| True      -> False
	| False     -> True
	| Eq (r, v) -> dnf_of (Ne (r, v)) vars env
	| Ne (r, v) -> dnf_of (Eq (r, v)) vars env
	| Not c1    -> dnf_of c1 vars env
	| Imply (premise, conclusion) ->
		dnf_of (And [premise; (Not conclusion)]) vars env                (* -(p -> q) = p ^ -q *)
	| And cs ->
		let cs1 = List.fold_left (fun acc c -> (Not c) :: acc) [] cs in  (* De Morgan's laws *)
		dnf_of (Or cs1) vars env
	| Or cs  ->
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
	;;

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
	;;


(************************************************************************
 * Functions for global constraints.
 ************************************************************************)

type constraints_variables = { cons: _constraint list; implies: _constraint list; vars: Variable.ts }

(** compile simple membership of global constraints **)
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
					(* below lines were commented because the above TODO has not been implemented *)
					if Variable.mem r2 acc.vars then
						{ cons = acc.cons; implies = c :: acc.implies; vars = acc.vars }
					else
						{ cons = c :: acc.cons; implies = acc.implies; vars = acc.vars }
				| _              -> error 508
			)
			{ cons = cons_vars.cons; implies = cons_vars.implies; vars = vars1 }
			(Variable.values_of prevail vars1)

(** compile simple equality of global constraints **)
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
					(* below lines were commented because the above TODO has not been implemented *)
					if Variable.mem r2 acc.vars then
						{ cons = acc.cons; implies = c :: acc.implies; vars = acc.vars }
					else
						{ cons = c :: acc.cons; implies = acc.implies; vars = acc.vars }
				| _ -> error 510
			)
			{ cons = cons_vars.cons; implies = cons_vars.implies; vars = vars1 }
			(Variable.values_of prevail vars1)

(** compile simple equality and membership of global constraints **)
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
let global_of (env: Sftype.env) (fs: Sfdomain.flatstore) (vars: Variable.ts) : (_constraint * _constraint list * Variable.ts) =
	let r = ["global"] in
	if MapRef.mem r fs then
		match MapRef.find r fs with
		| Global g ->
			(* let (global1, implies1, vars1) = compile_simple_global g vars env in
			let global_dnf = dnf_of global1 vars1 env in
			(global_dnf, implies1, vars1) *)
			(dnf_of g vars env, [], vars)
		| _        -> error 519
	else (True, [], vars)
