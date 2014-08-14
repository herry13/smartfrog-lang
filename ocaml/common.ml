module MapStr = Map.Make(String)

module MapRef = Map.Make ( struct
	type t = string list
	let compare = Pervasives.compare
end )

module SetRef = Set.Make ( struct
	type t = string list
	let compare = Pervasives.compare
end )
