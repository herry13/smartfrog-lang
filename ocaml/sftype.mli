open Sfsyntax

(*******************************************************************
 * type environment
 *******************************************************************)

type _t = Type of _type
        | Undefined
and var = string list * _type
and env = var list

(*******************************************************************
 * public functions
 *******************************************************************)

exception TypeError of int * string

val string_of_env : env -> string

val sfpSpecification : Sfsyntax.sfp -> env
