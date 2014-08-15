open Common
open Sfdomain

val eval : store -> _constraint -> bool

val dnf_of : _constraint -> Variable.ts -> Sftype.env -> _constraint

val substitute_parameters_of : _constraint -> basic MapStr.t -> _constraint

val global_of : Sftype.env -> Sfdomain.flatstore -> Variable.ts -> (_constraint * _constraint list * Variable.ts)
