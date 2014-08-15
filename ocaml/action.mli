open Common
open Sfdomain

(** name * parameters * cost * preconditions * effects **)
type t = reference * basic MapStr.t * cost * basic MapRef.t * basic MapRef.t

val encode_name : reference -> basic MapStr.t -> string

val json_of_actions : t list -> string

val ground_actions : Sftype.env -> Variable.ts -> Sftype.typevalue -> _constraint -> _constraint list -> t list
