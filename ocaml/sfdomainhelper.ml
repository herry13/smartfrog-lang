(*******************************************************************
 * helper functions to convert semantics domain to YAML, JSON,
 * plain SF, or XML
 *******************************************************************)

open Sfdomain

(***
 * convert reference (list of string) to string
 ***)	
let string_of_ref r = "$." ^ String.concat ":" r

(***
 * convert a store to YAML
 ***)
let rec yaml_of_store s = yaml_of_store1 s ""

and yaml_of_store1 s tab =
	match s with
	| [] -> "{}"
	| (ids,vs) :: tail ->
		let h = tab ^ ids ^ ": " in
		match vs with
		| Basic basic ->
			let v = h ^ yaml_of_basic basic in
			if tail = [] then v else v ^ "\n" ^ yaml_of_store1 tail tab
		| Store child ->
			let v = h ^ (if child = [] then "" else "\n") ^ yaml_of_store1 child (tab ^ "  ") in
			if tail = [] then v else v ^ "\n" ^ yaml_of_store1 tail tab

and yaml_of_vec vec =
	match vec with
	| [] -> ""
	| head :: tail ->
		let v = yaml_of_basic head in
		if tail = [] then v else v ^ "," ^ (yaml_of_vec tail)

and yaml_of_basic v =
	match v with
	| Boolean b -> string_of_bool b
	| Number (Int i) -> string_of_int i
	| Number (Float f) -> string_of_float f
	| String s -> s
	| Null -> "null"
	| Vector vec -> "[" ^ (yaml_of_vec vec) ^ "]"
	| Ref r -> string_of_ref r
	| Link lr -> "link " ^ string_of_ref lr

(***
 * convert a store to a plain SF
 ***)
and sf_of_store s = sf_of_store1 s ""

and sf_of_store1 s tab =
	match s with
	| [] -> ""
	| (ids,vs) :: tail ->
		let h = tab ^ ids ^ " " in
		match vs with
		| Basic basic ->
			let v = h ^ (sf_of_basic basic) ^ ";" in
			if tail = [] then v else v ^ "\n" ^ sf_of_store1 tail tab
		| Store child ->
			let v =
				h ^ "extends  " ^
				(if child = [] then "{}" else "{\n" ^ (sf_of_store1 child (tab ^ "  ")) ^ "\n" ^ tab ^ "}") in
			if tail = [] then v else v ^ "\n" ^ sf_of_store1 tail tab

and sf_of_vec vec =
	match vec with
	| [] -> ""
	| head :: tail ->
		let v =
			sf_of_basic head in
			if tail = [] then v else v ^ ", " ^ (sf_of_vec tail)

and sf_of_basic v =
	match v with
	| Boolean b -> string_of_bool b
	| Number (Int i) -> string_of_int i
	| Number (Float f) -> string_of_float f
	| String s -> s
	| Null -> "null"
	| Vector vec -> "[" ^ (sf_of_vec vec) ^ "]"
	| Ref r -> "DATA " ^ String.concat ":" r
	| Link lr -> " " ^ String.concat ":" lr

(***
 * convert a store to JSON
 ***)
and json_of_store s = "{" ^ (json_of_store1 s) ^ "}"

and json_of_store1 s =
	match s with
	| [] -> ""
	| (ids,vs) :: tail ->
		let h = "\"" ^ ids ^ "\":" in
		match vs with
		| Basic basic ->
			let v = h ^ json_of_basic basic in
			if tail = [] then v else v ^ "," ^ json_of_store1 tail
		| Store child ->
			let v = h ^ "{" ^ (json_of_store1 child) ^ "}" in
			if tail = [] then v else v ^ "," ^ json_of_store1 tail

and json_of_basic v =
	match v with
	| Boolean b -> string_of_bool b
	| Number (Int i) -> string_of_int i
	| Number (Float f) -> string_of_float f
	| String s -> "\"" ^ s ^ "\""
	| Null -> "null"
	| Vector vec -> "[" ^ (json_of_vec vec) ^ "]"
	| Ref r -> "\"" ^ (string_of_ref r) ^ "\""
	| Link lr -> "\"link " ^(string_of_ref lr) ^ "\""

and json_of_vec vec =
	match vec with
	| [] -> ""
	| head :: tail ->
		let h = json_of_basic head in
		if tail = [] then h else h ^ "," ^ (json_of_vec tail)

(***
 * convert a store to XML
 * generate XML of given store
 * - attribute started with '_' is treated as parent's XML attribute
 * - attribute started without '_' is treated as element
 ***)
and xml_of_store s : string = xml_of_store1 s

and xml_of_store1 s : string =
	match s with
	| [] -> ""
	| (ids,vs) :: tail ->
		if (String.get ids 0) = '_' then xml_of_store1 tail
		else
			match vs with
			| Basic basic ->
				let h = "<" ^ ids ^ ">" ^ (xml_of_basic basic) ^ "</" ^ ids ^ ">" in
				if tail = [] then h else h ^ "\n" ^ xml_of_store1 tail
			| Store child ->
				let h = "<" ^ ids ^ (attribute_of_store child) ^ ">" ^ (xml_of_store1 child) ^ "</" ^ ids ^ ">" in
				if tail = [] then h else h ^ "\n" ^ xml_of_store1 tail

and attribute_of_store s : string =
	let attr = String.trim (accumulate_attribute s) in
	if (String.length attr) > 0 then " " ^ attr else attr

and is_attribute id = ((String.get id 0) = ' ')

and accumulate_attribute s : string =
	match s with
	| [] -> ""
	| (ids,vs) :: tail ->
		if is_attribute ids then
			match vs with
			| Store _ | Basic (Vector _) -> raise (Failure "XML attr may not a component or vector")
			| Basic b -> " " ^ string_of_attribute ids b ^ accumulate_attribute tail
		else accumulate_attribute tail

and string_of_attribute id v =
	(String.sub id 1 ((String.length id) - 1)) ^ "=\"" ^ xml_of_basic v ^ "\""

and xml_of_basic v : string =
	match v with
	| Boolean b -> string_of_bool b
	| Number (Int i) -> string_of_int i
	| Number (Float f) -> string_of_float f
	| String s -> s
	| Null -> "</null>"
	| Vector vec -> "<vector>" ^ (xml_of_vec vec) ^ "</vector>"
	| Ref r -> string_of_ref r
	| Link lr -> "<link>" ^ (string_of_ref lr) ^ "</link>"

and xml_of_vec vec : string =
	match vec with
	| [] -> ""
	| head :: tail -> "<item>" ^ (xml_of_basic head) ^ "</item>" ^ xml_of_vec tail;;

