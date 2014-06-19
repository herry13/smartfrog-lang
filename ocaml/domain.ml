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
            | DataRef of string list
            | Vec of vector
            | LazyRef of string list
and value   = Basic of basic
            | Store of store
            | LR of string list
            | TBD
and _value  = Val of value
            | Undefined
and cell    = { id : string; v : value }
and store   = cell list;;

(*** helper functions ***)

let string_of_ref r = "$." ^ String.concat ":" r

let random_ident length =
  let gen() = match Random.int(52) with
    | x when x < 26 -> int_of_char 'a' + x
    | x -> int_of_char 'A' + x - 26 in
  let gen _ = String.make 1 (char_of_int(gen())) in
  String.concat "" (Array.to_list (Array.init length gen))

let is_schema_ident id =
  if (String.length id) > 6 then ((String.sub id ((String.length id)-6) 6) = "Schema")
  else false

let failure code =
  prerr_string ("[err" ^ string_of_int(code) ^ "]\n");
  exit code

(* 
 * semantics algebras
 *)

let ref_main = ["sfConfig"]

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
let (+) (id1 : string) (id2 : string) = [id1; id2];;
 
(* only applicable to oplus (string -> string list -> string list) *)
let (!+) (id : string) (r : string list) = id :: r;;

let (+!) (r : string list) (id : string) = r@[id];;

(* only applicable to oplus (string list -> string list -> string list) *)
let rec (++) (r1 : string list) (r2 : string list) =
  match r1 with
  | [] -> r2
  | id::rs -> if r2 = [] then r1
              else id :: (rs ++ r2);;

let rec (<+) (left : string list) (right : string list) =
  match right with
  | [] -> left
  | head :: tail ->
      if head = "ATTRIB" then left <+ tail
      else if head = "THIS" then left <+ tail
      else if head = "ROOT" then [] <+ tail
      else if head = "PARENT" then
        if left = [] then failure 104
        else (prefix left) <+ tail
      else (left +! head) <+ tail

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

(* r1 is a prefix or equals r2 *)
let rec (<=) r1 r2 : bool = if (r1 -- r2) = [] then true else false;;

(* r1 is a strict prefix of r2 *)
let rec (<) r1 r2 : bool = (r1 <= r2) && not (r1 == r2);;

module SetRef = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = string list
  end )


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
  if ns = [] then ([], find s ([] <+ r))
  else
    let v = find s (ns <+ r) in
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
  | nsp, Val (LR lr) -> ( match (resolve_link s nsp r lr) with
                          | nslr, Val (Store s1) ->
                              if (nslr <+ lr) <= r then failure 41 (* cyclic prototype - to be compatible with production *)
                              else copy s s1 r
                          | _, _ -> failure 4 (* invalid prototype *)
                        )
  | nsp, Val (Store vp) ->
      if (nsp <+ proto) <= r then failure 42 (* cyclic prototype - to be compatible with production *)
      else copy s vp r
  | _, _ -> failure 43 (* invalid prototype *)

and resolve_link s ns r lr = get_link s ns r lr SetRef.empty

and get_link s ns r lr acc =
  if SetRef.exists (fun r1 -> r1 = lr) acc then failure 20 (* cyclic link reference *)
  else
    match resolve s ns lr with
    | nsp, Val (LR lrp) -> get_link s (prefix (nsp <+ lr)) r lrp (SetRef.add lr acc)
    | nsp, vp -> if (nsp <+ lr) <= r then failure 21 (* implicit cyclic link reference *)
                 else (nsp <+ lr, vp)

and replace_link root ns c nslr =
  match c.v with
  | LR lr -> (
               match resolve_link root nslr (ns +! c.id) lr with
               | _, Undefined -> failure 22 (* invalid link-reference *)
               | nsp, Val (Store sp) ->
                   let rootp = bind root (ns +! c.id) (Store sp) in
                   accept rootp (ns +! c.id) sp nsp
               | nsp, Val vp -> bind root (ns +! c.id) vp
             )
  | Store s -> accept root (ns +! c.id) s (ns +! c.id)
  | _ -> root

and accept root ns s nslr =
  match s with
  | [] -> root
  | head :: tail ->
      let root1 = replace_link root ns head nslr in
      accept root1 ns tail nslr

(*
 * helper functions : domain to string conversion
 *)

(*** generate YAML from given store ***)
and yaml_of_store s = yaml_of_store1 s ""

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
      | LR lr ->
          let v = h ^ "link[" ^ (string_of_ref lr) ^ "]" in
          if tail = [] then v else v ^ "\n" ^ yaml_of_store1 tail tab
      | TBD ->
          let v = h ^ "TBD" in
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
  | DataRef r -> string_of_ref r
  | LazyRef r -> "@." ^ String.concat ":" r
  | Vec vec -> "[" ^ (yaml_of_vec vec) ^ "]"

(*** generate plain SF of given store ***)
and sf_of_store s = sf_of_store1 s ""

and sf_of_store1 s tab =
  match s with
  | [] -> ""
  | head :: tail ->
      if is_schema_ident(head.id) then sf_of_store1 tail tab else
      let h = tab ^ head.id ^ " " in
      match head.v with
      | Basic basic ->
          let v = h ^ (sf_of_basic basic) ^ ";" in
          if tail = [] then v else v ^ "\n" ^ sf_of_store1 tail tab
      | Store child ->
          let v = h ^ "extends  " ^ (if child = [] then "{}" else "{\n" ^ (sf_of_store1 child (tab ^ "  ")) ^ "\n" ^ tab ^ "}") in
          if tail = [] then v else v ^ "\n" ^ sf_of_store1 tail tab
      | LR lr -> let v = h ^ (String.concat ":" lr) in
                 if tail = [] then v else v ^ "," ^ json_of_store1 tail
      | TBD -> let v = h ^ "TBD" in
               if tail = [] then v else v ^ "," ^ json_of_store1 tail

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
  | Str s -> "\"" ^ s ^ "\""
  | Null -> "NULL"
  | DataRef r -> "DATA " ^ String.concat ":" r
  | LazyRef r -> "LAZY " ^ String.concat ":" r
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
      | LR lr -> let v = h ^ "link[" ^ (string_of_ref lr) ^ "]" in
                 if tail = [] then v else v ^ "," ^ json_of_store1 tail
      | TBD -> let v = h ^ "TBD" in
               if tail = [] then v else v ^ "," ^ json_of_store1 tail

and json_of_basic v =
  match v with
  | Bool b -> string_of_bool b
  | Num (Int i) -> string_of_int i
  | Num (Float f) -> string_of_float f
  | Str s -> "\"" ^ s ^ "\""
  | Null -> "null"
  | DataRef r -> "\"" ^ (string_of_ref r) ^ "\""
  | LazyRef r -> "@." ^ String.concat ":" r
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
        | LR lr ->
            let h = "<" ^ head.id ^ "><link>" ^ (string_of_ref lr) ^ "</link></" ^ head.id ^ ">" in
            if tail = [] then h else h ^ "\n" ^ xml_of_store1 tail
        | TBD ->
            let h = "<" ^ head.id ^ "><tbd/></" ^ head.id ^ ">" in
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
        | Store _ | Basic (Vec _) -> failure 101 (* XML attr may not a component or vector *)
        | Basic b -> " " ^ string_of_attribute head.id b ^ accumulate_attribute tail
        | LR lr -> failure 102 (* link-reference *)
        | TBD   -> failure 103 (* TBD *)
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
  | DataRef r -> string_of_ref r
  | LazyRef r -> "@." ^ String.concat ":" r
  | Vec vec -> "<vector>" ^ (xml_of_vec vec) ^ "</vector>"

and xml_of_vec vec : string =
  match vec with
  | [] -> ""
  | head :: tail -> "<item>" ^ (xml_of_basic head) ^ "</item>" ^ xml_of_vec tail;;

