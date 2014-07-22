(***
 * sfhelper.ml - helper functions
 * author: Herry (herry13@gmail.com)
 *
 * changelog:
 * 22.07.2014 - first released
 ***)

open Sfdomain

(*******************************************************************
 * lexer helper type and functions
 *******************************************************************)

(***
 * The Lexstack type.
 ***)
type 'a t =
	{
		mutable stack    : (string * in_channel * Lexing.lexbuf) list;
		mutable filename : string;
		mutable chan     : in_channel;
		mutable lexbuf   : Lexing.lexbuf;
		lexfunc          : Lexing.lexbuf -> 'a;
	}

(***
 * Create a lexstack with an initial top level filename and the lexer function
 ***)
let create top_filename lexer_function =
	let chan = open_in top_filename in
	{
		stack = [];
		filename = top_filename;
		chan = chan;
		lexbuf = Lexing.from_channel chan;
		lexfunc = lexer_function
	}

(***
 * Get the current lexeme.
 ***)
let lexeme ls = Lexing.lexeme ls.lexbuf

(***
 * Get filename, line number and column number of current lexeme.
 ***)
let current_pos ls =
	let pos = Lexing.lexeme_end_p ls.lexbuf in
	let linepos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - String.length (Lexing.lexeme ls.lexbuf) in
	ls.filename, pos.Lexing.pos_lnum, linepos

(***
 * The next token need to accept an unused dummy lexbuf so that
 * a closure consisting of the function and a lexstack can be passed
 * to the ocamlyacc generated parser.
 ***)
let rec get_token ls dummy_lexbuf =
	let token = ls.lexfunc ls.lexbuf in
	match token with
	| Sfparser.INCLUDE file -> (* parse included file *)
		(* uncomment below codes to use C-style include (the included file may be illegal statements) *)
		(*
		ls.stack <- (ls.filename, ls.chan, ls.lexbuf) :: ls.stack;
		ls.filename <- file;
		ls.chan <- open_in file;
		ls.lexbuf <- Lexing.from_channel ls.chan;
		get_token ls dummy_lexbuf
		*)
  
		(* this SF-style include (the included file must be legal statements) **)
		Sfparser.SF_INCLUDE
		(
			fun ns s ->
				let lexstack = create file Sflexer.token in
				try 
					Sfparser.included (get_token lexstack) dummy_lexbuf ns s
				with e -> check_error e lexstack
		)
	| Sfparser.EOF ->
		(
			match ls.stack with
			| [] -> Sfparser.EOF      (* buffer is empty, then return EOF *)
			| (fn, ch, lb) :: tail -> (* buffer isn't empty, then continue to parse top file *) 
				ls.filename <- fn;
				ls.chan <- ch;
				ls.stack <- tail;
				ls.lexbuf <- lb;
				get_token ls dummy_lexbuf
		)
  | _ -> token

(***
 * Catch the exception, when it is a parse error (Parse_error)
 * then throw a failure containing a string of error message,
 * otherwise just throw again the exception
 *
 * TODO - sometime error line and column number does not match
 ***)
and check_error e lexstack =
	match e with
	| Parsing.Parse_error ->
		let fname, lnum, lpos = current_pos lexstack in
		let errstr = Printf.sprintf
			"\n\nFile '%s' line %d, column %d\ncurrent token is '%s'.\n\n"
			fname lnum lpos (lexeme lexstack) in
		raise (Failure errstr)
	| e -> raise e
	

(*******************************************************************
 * helper functions to convert semantics domain to YAML, JSON, plain SF, or XML
 *******************************************************************)

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
	| Bool b -> string_of_bool b
	| Num (Int i) -> string_of_int i
	| Num (Float f) -> string_of_float f
	| Str s -> s
	| Null -> "null"
	| Ref r -> string_of_ref r
	| Vec vec -> "[" ^ (yaml_of_vec vec) ^ "]"

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
	| Bool b -> string_of_bool b
	| Num (Int i) -> string_of_int i
	| Num (Float f) -> string_of_float f
	| Str s -> s
	| Null -> "null"
	| Ref r -> "DATA " ^ String.concat ":" r
	| Vec vec -> "[|" ^ (sf_of_vec vec) ^ "|]"

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
	| Bool b -> string_of_bool b
	| Num (Int i) -> string_of_int i
	| Num (Float f) -> string_of_float f
	| Str s -> "\"" ^ s ^ "\""
	| Null -> "null"
	| Ref r -> "\"" ^ (string_of_ref r) ^ "\""
	| Vec vec -> "[" ^ (json_of_vec vec) ^ "]"

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
			| Store _ | Basic (Vec _) -> raise (Failure "XML attr may not a component or vector")
			| Basic b -> " " ^ string_of_attribute ids b ^ accumulate_attribute tail
		else accumulate_attribute tail

and string_of_attribute id v =
	(String.sub id 1 ((String.length id) - 1)) ^ "=\"" ^ xml_of_basic v ^ "\""

and xml_of_basic v : string =
	match v with
	| Bool b -> string_of_bool b
	| Num (Int i) -> string_of_int i
	| Num (Float f) -> string_of_float f
	| Str s -> s
	| Null -> "</null>"
	| Ref r -> string_of_ref r
	| Vec vec -> "<vector>" ^ (xml_of_vec vec) ^ "</vector>"

and xml_of_vec vec : string =
	match vec with
	| [] -> ""
	| head :: tail -> "<item>" ^ (xml_of_basic head) ^ "</item>" ^ xml_of_vec tail;;

