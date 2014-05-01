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

(* only applicable to oplus (string list) (string) *)
let rec oplus r id =
  match r with
  | [] -> [id]
  | head::tail -> head :: oplus tail id;;

let rec ominus r1 r2 =
  if r1 = [] then []
  else if r2 = [] then r1
  else if (List.hd r1) = (List.hd r2) then ominus (List.tl r1) (List.tl r2)
  else r1;;

let rec equiv r1 r2 : bool =
  if r1 = [] then
    if r2 = [] then true else false
  else if r2 = [] then false
  else if (List.hd r1) = (List.hd r2) then equiv (List.tl r1) (List.tl r2)
  else false

let rec subeqref r1 r2 : bool = if ominus r1 r2 = [] then true else false;;

let rec subref r1 r2 : bool = (subeqref r1 r2) && not (equiv r1 r2);;

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
      else find tail r;;

let rec resolve s ns r =
  if ns = [] then ([], find s r)
  else
    let v = find s (List.append ns r) in
    match v with
    | Undefined -> resolve s (prefix ns) r
    | _ -> (ns, v);;

let rec put s id v =
  match s with
  | [] -> { id = id; v = v } :: []
  | head::tail ->
      if head.id = id then { id = id; v = v } :: tail
      else head :: put tail id v;;

let rec bind s r v : store =
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
            else head :: bind tail r v;;

let rec copy s1 s2 pfx =
  match s2 with
  | [] -> s1
  | head::tail -> copy (bind s1 (oplus pfx head.id) head.v) tail pfx;;

let inherit_proto s ns proto r =
  match resolve s ns proto with
  | nsp, Val (Store vp) -> copy s vp r
  | _, _ -> raise (Failure "[err4]");;



(*
 * helper functions : domain to string conversion
 *)

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

