(** common data structures **)

module MapStr : Map.S with type key = string

module MapRef : Map.S with type key = string list

module SetRef : Set.S with type elt = string list
