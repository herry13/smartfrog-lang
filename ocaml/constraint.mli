open Common
open Domain

val eval : store -> _constraint -> bool

val dnf_of : _constraint -> Variable.ts -> Type.env -> _constraint

val substitute_parameters_of : _constraint -> basic MapStr.t -> _constraint

val global_of : Type.env -> Domain.flatstore -> Variable.ts -> (_constraint * _constraint list * Variable.ts)
