(*
 * semantics primary and secondary domains
 *)
type number = Int of int | Float of float
and vector = value list
and value = Bool of bool | Num of number | Str of string | Null | Ref of string list | Vec of vector | Store of store
and _value = Val of value | Undefined
and cell = { id : string; v : value }
and store = cell list;;

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
      if head.id = id then { id = id; v = v } :: tail
      else head :: put tail id v

and copy dest src pfx : store =
  match src with
  | [] -> dest
  | head::tail -> copy (bind dest (pfx +! head.id) head.v) tail pfx

and bind s r v : store =
  match r with
  | [] -> raise (Failure "[err3]")
  | id :: rs ->
      if rs = [] then put s id v
      else
        match s with
        | [] -> raise (Failure "[err2]")
        | head::tail ->
            if head.id = id then
              match head.v with
              | Store child -> { id = id; v = (Store (bind child rs v)) } :: tail
              | _ -> raise (Failure "[err1]")
            else head :: bind tail r v

and inherit_proto s ns proto r : store =
  match resolve s ns proto with
  | nsp, Val (Store vp) -> copy s vp r
  | _, _ -> raise (Failure "[err4]");;



(*
 * helper functions : domain to string conversion
 *)

(*** generate YAML from given store ***)
let rec yaml_of_store s = yaml_of_store1 s ""
and yaml_of_vec vec =
  match vec with
  | [] -> ""
  | head::tail -> let v = yaml_of_value head "" in
                  if tail = [] then v else v ^ "," ^ (yaml_of_vec tail)
and yaml_of_value v tab =
  match v with
  | Bool b -> string_of_bool b
  | Num (Int i) -> string_of_int i
  | Num (Float f) -> string_of_float f
  | Str s -> s
  | Null -> "null"
  | Ref r -> "$." ^ String.concat ":" r
  | Vec vec -> "[" ^ (yaml_of_vec vec) ^ "]"
  | Store c ->
      if c = [] then "{}" else "\n" ^ yaml_of_store1 c (tab ^ "  ")
and yaml_of_store1 s tab =
  match s with
  | [] -> "{}"
  | head::tail ->
      let h = tab ^ head.id ^ ": " ^ yaml_of_value head.v (tab ^ "  ") in
      if tail = [] then h
      else h ^ "\n" ^ yaml_of_store1 tail tab;;

(*** generate JSON of given store ***)
let rec json_of_store s = "{" ^ (json_of_store1 s) ^ "}"
and json_of_store1 s =
  match s with
  | [] -> ""
  | head::tail ->
      let h = "\"" ^ head.id ^ "\":" ^ json_of_value head.v in
      if tail = [] then h
      else h ^ "," ^ json_of_store1 tail
and json_of_value v =
  match v with
  | Bool b -> string_of_bool b
  | Num (Int i) -> string_of_int i
  | Num (Float f) -> string_of_float f
  | Str s -> "\"" ^ s ^ "\""
  | Null -> "null"
  | Ref r -> "$." ^ String.concat ":" r
  | Vec vec -> "[" ^ (json_of_vec vec) ^ "]"
  | Store s -> "{" ^ (json_of_store1 s) ^ "}"
and json_of_vec vec =
  match vec with
  | [] -> ""
  | head :: tail -> let h = json_of_value head in
                    if tail = [] then h else h ^ "," ^ (json_of_vec tail);;

(*
 * generate XML of given store
 * - attribute started with '_' is treated as parent's XML attribute
 * - attribute started without '_' is treated as element
 *)
let rec xml_of_store s : string = xml_of_store1 s
and xml_of_store1 s : string =
  match s with
  | [] -> ""
  | head::tail ->
      if (String.get head.id 0) = '_' then xml_of_store1 tail
      else
        let h = "<" ^ head.id ^ (attribute_of_value head.v) ^ ">" ^ xml_of_value head.v ^ "</" ^ head.id ^ ">" in
        if tail = [] then h
        else h ^ "\n" ^ xml_of_store1 tail
and attribute_of_value v : string =
  match v with
  | Store s -> let attr = String.trim (accumulate_attribute s) in
               if (String.length attr) > 0 then " " ^ attr else attr
  | _ -> ""
and accumulate_attribute s : string =
  match s with
  | [] -> ""
  | head::tail ->
      if (String.get head.id 0) = '_' then
        match head.v with
        | Store _ | Ref _ -> raise (Failure "attribute may not be a component or vector")
        | _ -> " " ^ (String.sub head.id 1 ((String.length head.id) - 1)) ^ "=\"" ^ xml_of_value head.v ^ "\"" ^ accumulate_attribute tail
      else accumulate_attribute tail
and xml_of_value v : string =
  match v with
  | Bool b -> string_of_bool b
  | Num (Int i) -> string_of_int i
  | Num (Float f) -> string_of_float f
  | Str s -> s
  | Null -> "</null>"
  | Ref r -> "$." ^ String.concat ":" r
  | Vec vec -> "<vector>" ^ (xml_of_vec vec) ^ "</vector>"
  | Store s -> "\n" ^ xml_of_store1 s
and xml_of_vec vec : string =
  match vec with
  | [] -> ""
  | head::tail -> "<item>" ^ (xml_of_value head) ^ "</item>" ^ xml_of_vec tail;;

