open Common
open Domain

(*******************************************************************
 * functions for translating SFP to FDR
 *******************************************************************)

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
			Buffer.add_string buf "Atom (";
			Buffer.add_string buf !^(Variable.name var);
			Buffer.add_char buf ' ';
			Buffer.add_string buf (json_of_value v);
			Buffer.add_string buf ")\n";
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
 * Generate the FDR mutex
 *)
let of_mutex (buf: Buffer.t) (vars: Variable.ts) : unit =
	Buffer.add_char buf '\n';
	Buffer.add_string buf (string_of_int (Variable.total vars));
	Variable.iter (fun var ->
		Buffer.add_string buf "\nbegin_mutex_group\n";
		let index = string_of_int (Variable.index var) in
		Buffer.add_string buf (string_of_int (Variable.size var));
		Buffer.add_char buf '\n';
		Variable.iteri_values (fun i _ ->
			Buffer.add_string buf index;
			Buffer.add_char buf ' ';
			Buffer.add_string buf (string_of_int i);
			Buffer.add_char buf '\n';
		) var;
		Buffer.add_string buf "end_mutex_group";
	) vars;;

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
	Buffer.add_string buf (Action.encode_name name params);
	Buffer.add_char buf '\n';
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

let of_sfp (ast_0: Syntax.sfp) (ast_g: Syntax.sfp) : string =
	(* step 0: parse the specification and generate a store *)
	let env_0 = Type.sfpSpecification ast_0 in
	let store_0 = Valuation.sfpSpecification ast_0 in
	let env_g = Type.sfpSpecification ast_g in
	let store_g = Valuation.sfpSpecification ast_g in
	(* step 1: generate flat-stores of current and desired specifications *)
	let fs_0 = normalise store_0 in
	let fs_g = normalise store_g in
	(* step 1a: generate a type->value map *)
	let tvalues = Type.make_typevalue env_0 fs_0 env_g fs_g in
	(* step 2: translate *)
	(* step 2.1: generate variables *)
	let vars = Variable.make_ts env_0 fs_0 env_g fs_g tvalues in
	(* step 2.2: global constraints *)
	let (global, g_implies, vars1) = Constraint.global_of env_g fs_g vars in
	(* step 2.3 & 2.4: generate actions & compile global constraints *)
	let actions = Action.ground_actions env_g vars1 tvalues global g_implies in
	(* step 2.5: generate FDR *)
	let buffer = Buffer.create (100 + (40 * (Variable.total vars) * 2)) in
	of_header buffer;
	of_variables buffer vars1;
	of_mutex buffer vars1;
	of_init buffer vars1;
	of_goal buffer vars1 (global <> True);
	of_actions buffer actions vars1;
	of_axioms buffer;
	Buffer.contents buffer
