type number = Int of int | Float of float
and reference = string list
and value = Bool of bool | Num of number | Str of string | Null | Ref of reference | Store of store
and _value = Val of value | Undefined
and cell = { id : string; v : value }
and store = cell list;;

type epsilon = Epsilon;;

(* helper functions : domain to string conversion *)

let string_of_reference r = String.concat ":" r;;

let string_of_number n =
  match n with
  | Int i -> string_of_int i
  | Float f -> string_of_float f;;

let rec string_of_value v =
  match v with
  | Bool b -> string_of_bool b
  | Num n -> string_of_number n
  | Str s -> s
  | Ref r -> string_of_reference r
  | Null -> "null"
  | _ -> raise (Failure "invalid value");;

let string_of_cell c =
  match c with
  | { id = s; v = x; } -> s ^ ": " ^ string_of_value x;;

let rec string_of_store s = string_of_store1 s ""
and string_of_store1 s tab =
  match s with
  | [] -> ""
  | head::tail ->
     match head.v with
     | Store child ->
         if child = [] then tab ^ head.id ^ ": {}"
         else tab ^ head.id ^ ":\n" ^ string_of_store1 child (tab ^ "  ")
     | _ ->
         if tail = [] then tab ^ string_of_cell head
         else tab ^ string_of_cell head ^ "\n" ^ string_of_store1 tail tab;;


(* semantics algebras *)

(* reference functions *)
let rec prefix r =
  match r with
  | [] -> []
  | head::tail -> if tail = [] then head :: [] else head :: prefix tail;;

let rec ominus r1 r2 =
  if r1 = [] then []
  else if r2 = [] then r1
  else if (List.hd r1) = (List.hd r2) then ominus (List.tl r1) (List.tl r2)
  else r1;;

let rec oplus r1 r2 =
  match r1 as
  | string s -> s :: r2
 
  List.append r1 r2;;

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
