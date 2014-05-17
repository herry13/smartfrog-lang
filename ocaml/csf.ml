open Array

let help = "usage: csf [option] <sf-file>" ^
  "\n\nwhere [option] is:" ^
  "\n  -sf     print output in plain SF" ^
  "\n  -json   print output in JSON" ^
  "\n  -yaml   print output in YAML" ^
  "\n  -xml    print output in XML\n\n"

let compile inc opt file =
  let result = (if inc then Sf.parse_included file else Sf.parse file) in
  if opt = "-yaml" then print_string (Domain.yaml_of_store result)
  else if opt = "-xml" then print_string (Domain.xml_of_store result)
  else if opt = "-sf" then print_string (Domain.sf_of_store result)
  else print_string (Domain.json_of_store result);
  print_newline()

let _ =
  if (length Sys.argv) < 2 then print_string help
  else if Sys.argv.(1) = "-h" || Sys.argv.(1) = "--help" then print_string help
  else if (length Sys.argv) == 3 then compile false Sys.argv.(1) Sys.argv.(2)
  else if (length Sys.argv) == 4 && Sys.argv.(1) = "-i" then compile true Sys.argv.(2) Sys.argv.(3)
  else compile false "" Sys.argv.(1)
