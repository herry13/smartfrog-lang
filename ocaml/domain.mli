open Common

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

(** action elements : name * parameters * cost * preconditions * effects **)
and action         = reference * parameter_type list * cost * conditions * effect list
and parameter_type = ident * Syntax._type
and cost           = int
and conditions     = _constraint
and effect         = reference * basic

(*******************************************************************
 * semantics algebras
 *******************************************************************)

(** exception for any error on semantics algebra **)
exception SfError of int * string

(** a function that raise an SfError **)
val error : int -> 'a

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
 * domain convertion functions to string, JSON, or YAML
 *******************************************************************)

val (!^) : reference -> string

val yaml_of_store : store -> string

val json_of_store : store -> string

val json_of_value : value -> string

val json_of_constraint : _constraint -> string


(*******************************************************************
 * Flat-Store domain
 *******************************************************************)

type flatstore = value MapRef.t

val static_object : value

val normalise : store -> flatstore

val string_of_flatstore : flatstore -> string


(*******************************************************************
 * set of values
 *******************************************************************)

module SetValue : Set.S with type elt = value

val string_of_setvalue : SetValue.t -> string


(*******************************************************************
 * parameters
 *******************************************************************)

type ground_parameters = basic MapStr.t

val substitute_parameter_of_reference : reference -> ground_parameters -> reference

val substitute_parameter_of_basic_value : basic -> ground_parameters -> basic
