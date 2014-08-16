open Common

(*******************************************************************
 * type environment
 *******************************************************************)

type env = Syntax._type MapRef.t

(*******************************************************************
 * functions of type environment
 *******************************************************************)

exception TypeError of int * string

val string_of_env : env -> string

val sfpSpecification : Syntax.sfp -> env

val type_of : Domain.reference -> env -> Syntax._type


(*******************************************************************
 * a map from type to set of values
 *******************************************************************)

module MapType : Map.S with type key = Syntax._type

type typevalue = Domain.SetValue.t MapType.t

val values_of : Syntax._type -> typevalue -> Domain.SetValue.t

val add_value : Syntax._type -> Domain.value -> typevalue -> typevalue

val make_typevalue : env -> Domain.flatstore -> env -> Domain.flatstore -> Domain.SetValue.t MapType.t
