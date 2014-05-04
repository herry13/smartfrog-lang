
open Array

let help = "usage: csf [option] <sf-file>" ^
  "\n\nwhere [option] is:" ^
  "\n  -json   print output in JSON" ^
  "\n  -yaml   print output in YAML" ^
  "\n  -xml    print output in XML\n\n"

let parse file =
  let lexstack = Lexstack.create file Sflexer.token in
  let dummy_lexbuf = Lexing.from_string "" in
  try
    Sfparser.sf (Lexstack.get_token lexstack) dummy_lexbuf
  with e ->
    match e with
    | Parsing.Parse_error ->
        let fname, lnum, lpos = Lexstack.current_pos lexstack in
        let errstr = Printf.sprintf
          "\n\nFile '%s' line %d, column %d : current token is '%s'.\n\n"
          fname lnum lpos (Lexstack.lexeme lexstack) in
        raise (Failure errstr)
    | e -> raise e

let compile opt file =
  let result = parse file in
  if opt = "-yaml" then print_string (Domain.yaml_of_store result)
  else if opt = "-xml" then print_string (Domain.xml_of_store result)
  else print_string (Domain.json_of_store result);
  print_newline()

let _ =
	if (length Sys.argv) < 2 then print_string help
	else if Sys.argv.(1) = "-h" || Sys.argv.(1) = "--help" then print_string help
	else if (length Sys.argv) >= 3 then compile Sys.argv.(1) Sys.argv.(2)
	else compile "" Sys.argv.(1)
