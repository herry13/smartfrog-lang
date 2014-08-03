open Sfdomain

(*******************************************************************
 * semantics domain
 *******************************************************************)
type value   = Basic of Sfast._type
             | Store of store * Sfast._type
and  _value  = Val of value
             | Undefined
and  cell    = string * value
and  store   = cell list 

(** TODO **)

let find

let resolve

let put

let copy

let bind

let _inherit

let resolve_link

let get_link

let accept

let replace_link
