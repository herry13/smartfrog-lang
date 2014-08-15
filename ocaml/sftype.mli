open Common

(*******************************************************************
 * type environment
 *******************************************************************)

type env = Sfsyntax._type MapRef.t

(*******************************************************************
 * functions of type environment
 *******************************************************************)

exception TypeError of int * string

val string_of_env : env -> string

val sfpSpecification : Sfsyntax.sfp -> env

val type_of : Sfdomain.reference -> env -> Sfsyntax._type


(*******************************************************************
 * a map from type to set of values
 *******************************************************************)

module MapType : Map.S with type key = Sfsyntax._type

type typevalue = Sfdomain.SetValue.t MapType.t

val values_of : Sfsyntax._type -> typevalue -> Sfdomain.SetValue.t

val add_value : Sfsyntax._type -> Sfdomain.value -> typevalue -> typevalue

val make_typevalue : env -> Sfdomain.flatstore -> env -> Sfdomain.flatstore -> Sfdomain.SetValue.t MapType.t
