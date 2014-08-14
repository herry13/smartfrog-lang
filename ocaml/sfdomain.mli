(*******************************************************************
 * semantics domain
 *******************************************************************)
(** core elements **)
type number   = Int of int
              | Float of float
and vector    = basic list
and basic     = Boolean of bool
              | Number of number
              | String of string
              | Null
              | Vector of vector
              | Ref of reference
and value     = Basic of basic
              | Store of store
              | Global of _constraint
              | Link of reference
              | Action of action
and _value    = Val of value
              | Undefined
and cell      = ident * value
and store     = cell list
and reference = ident list
and ident     = string

(** constraint elements **)
and _constraint = Eq of reference * basic
                | Ne of reference * basic
                | Not of _constraint
                | Imply of _constraint * _constraint
                | And of _constraint list
                | Or of _constraint list
                | In of reference * vector
                | True
                | False

(** action elements **)
and action     = reference * parameters * cost * conditions * effects
and parameters = param list
and param      = ident * Sfsyntax._type
and cost       = int
and conditions = _constraint
and effects    = effect list
and effect     = reference * basic

(*******************************************************************
 * semantics algebras
 *******************************************************************)

(* identifier-reference functions *)

val prefix : reference -> reference

val (!-) : reference -> reference

val (@++) : reference -> reference -> reference

val (@+.) : reference -> string -> reference

val (@--) : reference -> reference -> reference

val (@<=) : reference -> reference -> bool

val (@<) : reference -> reference -> bool

val (@<<) : reference -> reference -> reference

val (!!) : reference -> reference

(* store functions *)

val find : store -> reference -> _value

val resolve : store -> reference -> reference -> (reference * _value)

val put : store -> string -> value -> store

val bind : store -> reference -> value -> store

val copy : store -> store -> reference -> store

val inherit_proto : store -> reference -> reference -> reference -> store

val accept : store -> reference -> store -> reference -> store


(*******************************************************************
 * helpers
 *******************************************************************)

(** exception for any error on semantics algebra **)
exception SfError of int * string

val error : int -> 'a

val (!^) : reference -> string

val yaml_of_store : store -> string

val json_of_store : store -> string

val json_of_value : value -> string

val json_of_constraint : _constraint -> string

