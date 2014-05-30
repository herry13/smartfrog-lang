(*
 * semantics primary and secondary domains
 *)
type number = Int of int
            | Float of float
and vector  = basic list
and basic   = Bool of bool
            | Num of number
            | Str of string
            | Null
            | Ref of string list
            | Vec of vector
and value   = Basic of basic
            | Store of store
and _value  = Val of value
            | Undefined
and cell    = { id : string; v : value }
and store   = cell list;;

(* helper functions *)
let failure code =
  prerr_string ("[err" ^ string_of_int(code) ^ "]\n");
  exit code

(* 
 * semantics algebras
 *)

(* reference functions *)
let rec prefix r =
  match r with
  | [] -> []
  | head::tail -> if tail = [] then [] else head :: (prefix tail);;

(* only applicable to oplus (string list -> string -> string list) *)
let rec (+!) (r : string list) (id : string) =
  match r with
  | [] -> [id]
  | head::tail -> head :: (tail +! id);;

(* only applicable to oplus (string -> string -> string list) *)
let (+) (id1 : string) (id2 : string) = id1 :: id2 :: [];;
 
(* only applicable to oplus (string -> string list -> string list) *)
let rec (!+) (id : string) (r : string list) = id :: r;;

(* only applicable to oplus (string list -> string list -> string list) *)
let rec (++) (r1 : string list) (r2 : string list) =
  match r1 with
  | [] -> r2
  | id::rs -> if r2 = [] then r1
              else id :: (rs ++ r2);;

let rec (--) r1 r2 =
  if r1 = [] then []
  else if r2 = [] then r1
  else if (List.hd r1) = (List.hd r2) then (List.tl r1) -- (List.tl r2)
  else r1;;

let rec (==) r1 r2 : bool =
  if r1 = [] then
    if r2 = [] then true else false
  else if r2 = [] then false
  else if (List.hd r1) = (List.hd r2) then (List.tl r1) == (List.tl r2)
  else false

let rec (<=) r1 r2 : bool = if (r1 -- r2) = [] then true else false;;

let rec (<) r1 r2 : bool = (r1 <= r2) && not (r1 == r2);;

(* store functions *)

let rec find s r : _value =
  match (s, r) with
  | (_, []) -> Val (Store s)
  | ([], _) -> Undefined
  | (head::tail, id::rs) ->
      if head.id = id then
        if rs = [] then Val head.v
        else match head.v with
        | Store child -> find child rs
        | _ -> Undefined
      else find tail r

and resolve s ns r =
  if ns = [] then ([], find s r)
  else
    let v = find s (List.append ns r) in
    match v with
    | Undefined -> resolve s (prefix ns) r
    | _ -> (ns, v)

and put s id v : store =
  match s with
  | [] -> { id = id; v = v } :: []
  | head::tail ->
      if head.id = id then
        match head.v, v with
        | Store dest, Store src -> { id = id; v = Store (copy dest src []) } :: tail
        | _,_ -> { id = id; v = v } :: tail
      else head :: put tail id v

and copy dest src pfx : store =
  match src with
  | [] -> dest
  | head::tail -> copy (bind dest (pfx +! head.id) head.v) tail pfx

and bind s r v : store =
  match r with
  | [] -> failure 3
  | id :: rs ->
      if rs = [] then put s id v
      else
        match s with
        | [] -> failure 2
        | head::tail ->
            if head.id = id then
              match head.v with
              | Store child -> { id = id; v = (Store (bind child rs v)) } :: tail
              | _ -> failure 1
            else head :: bind tail r v

and inherit_proto s ns proto r : store =
  match resolve s ns proto with
  | nsp, Val (Store vp) -> copy s vp r
  | _, _ -> failure 4



(*
 * helper functions : domain to string conversion
 *)

let string_of_ref r = "$." ^ String.concat ":" r

(*** generate YAML from given store ***)
let rec yaml_of_store s = yaml_of_store1 s ""

and yaml_of_store1 s tab =
  match s with
  | [] -> "{}"
  | head :: tail ->
      let h = tab ^ head.id ^ ": " in
      match head.v with
      | Basic basic ->
          let v = h ^ yaml_of_basic basic in
          if tail = [] then v else v ^ "\n" ^ yaml_of_store1 tail tab
      | Store child ->
          let v = h ^ (if child = [] then "" else "\n") ^ yaml_of_store1 child (tab ^ "  ") in
          if tail = [] then v else v ^ "\n" ^ yaml_of_store1 tail tab

and yaml_of_vec vec =
  match vec with
  | [] -> ""
  | head :: tail -> let v = yaml_of_basic head in
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

(*** generate plain SF of given store ***)
and sf_of_store s = sf_of_store1 s ""

and sf_of_store1 s tab =
  match s with
  | [] -> ""
  | head :: tail ->
      let h = tab ^ head.id ^ " " in
      match head.v with
      | Basic basic ->
          let v = h ^ (sf_of_basic basic) ^ ";" in
          if tail = [] then v else v ^ "\n" ^ sf_of_store1 tail tab
      | Store child ->
          let v = h ^ "extends  " ^ (if child = [] then "{}" else "{\n" ^ (sf_of_store1 child (tab ^ "  ")) ^ "\n" ^ tab ^ "}") in
          if tail = [] then v else v ^ "\n" ^ sf_of_store1 tail tab

and sf_of_vec vec =
  match vec with
  | [] -> ""
  | head :: tail -> let v = sf_of_basic head in
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



(*** generate JSON of given store ***)
and json_of_store s = "{" ^ (json_of_store1 s) ^ "}"

and json_of_store1 s =
  match s with
  | [] -> ""
  | head :: tail ->
      let h = "\"" ^ head.id ^ "\":" in
      match head.v with
      | Basic basic -> let v = h ^ json_of_basic basic in
                       if tail = [] then v else v ^ "," ^ json_of_store1 tail
      | Store child -> let v = h ^ "{" ^ (json_of_store1 child) ^ "}" in
                       if tail = [] then v else v ^ "," ^ json_of_store1 tail

and json_of_basic v =
  match v with
  | Bool b -> string_of_bool b
  | Num (Int i) -> string_of_int i
  | Num (Float f) -> string_of_float f
  | Str s -> "\"" ^ s ^ "\""
  | Null -> "null"
  | Ref r -> string_of_ref r
  | Vec vec -> "[" ^ (json_of_vec vec) ^ "]"

and json_of_vec vec =
  match vec with
  | [] -> ""
  | head :: tail -> let h = json_of_basic head in
                    if tail = [] then h else h ^ "," ^ (json_of_vec tail)

(*
 * generate XML of given store
 * - attribute started with '_' is treated as parent's XML attribute
 * - attribute started without '_' is treated as element
 *)
and xml_of_store s : string = xml_of_store1 s

and xml_of_store1 s : string =
  match s with
  | [] -> ""
  | head :: tail ->
      if (String.get head.id 0) = '_' then xml_of_store1 tail
      else
        match head.v with
        | Basic basic ->
            let h = "<" ^ head.id ^ ">" ^ (xml_of_basic basic) ^ "</" ^ head.id ^ ">" in
            if tail = [] then h else h ^ "\n" ^ xml_of_store1 tail
        | Store child ->
            let h = "<" ^ head.id ^ (attribute_of_store child) ^ ">" ^ (xml_of_store1 child) ^ "</" ^ head.id ^ ">" in
            if tail = [] then h else h ^ "\n" ^ xml_of_store1 tail

and attribute_of_store s : string =
  let attr = String.trim (accumulate_attribute s) in
  if (String.length attr) > 0 then " " ^ attr else attr

and is_attribute id = ((String.get id 0) = ' ')

and accumulate_attribute s : string =
  match s with
  | [] -> ""
  | head :: tail ->
      if is_attribute head.id then
        match head.v with
        | Store _ | Basic (Vec _) -> raise (Failure "XML attr may not a component or vector")
        | Basic b -> " " ^ string_of_attribute head.id b ^ accumulate_attribute tail
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

