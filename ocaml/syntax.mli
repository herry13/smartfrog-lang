(*******************************************************************
 * abstract syntax tree
 *******************************************************************)
type sf            = block
and  sfp           = sfpcontext
and  sfpcontext    = A_C of assignment * sfpcontext
                   | S_C of schema * sfpcontext
                   | G_C of global * sfpcontext
                   | EmptyContext
and  block         = A_B of assignment * block
                   | G_B of global * block
                   | EmptyBlock
and  assignment    = reference * _type * value
and  value         = BV  of basicValue
                   | LR  of reference
                   | P   of superSchema * prototype
                   | Ac  of action
and  prototype     = R_P of reference * prototype
                   | B_P of block * prototype
                   | EmptyPrototype
and  basicValue    = Boolean of string
                   | Number  of string
                   | String  of string
                   | Null
                   | Vector  of vector
                   | DR      of reference
and  vector        = basicValue list
and  reference     = string list

(** schema syntax **)
and schema      = string * superSchema * block
and superSchema = SID of string
                | EmptySchema

(** type syntax **)
and _type     = TBasic   of basicType
              | TVec     of _type
              | TRef     of basicType
              | TForward of reference * bool  (* r [link: true, data: false] *)
              | TUndefined
and basicType = TBool                         (* (Type Bool)   *)
              | TNum                          (* (Type Num)    *)
              | TStr                          (* (Type Str)    *)
              | TObject                       (* (Type Object) *)
              | TSchema of string * basicType (* (Type Schema) *)
              | TNull                         (* (Type Null)   *)
              | TAction                       (* (Type Action) *)
              | TGlobal                       (* (Type Global) *)
              | TRootSchema

(** constraint syntax **)
and global      = _constraint
and _constraint = Eq of reference * basicValue
                | Ne of reference * basicValue
                | Not of _constraint
                | Imply of _constraint * _constraint
                | And of _constraint list
                | Or of _constraint list
                | In of reference * vector

(** action syntax **)
and action     = parameters * cost * conditions * effects
and parameters = parameter list
and parameter  = string * _type
and cost       = Cost of string
               | EmptyCost
and conditions = Cond of _constraint
               | EmptyCondition
and effects    = effect list
and effect     = reference * basicValue

(*******************************************************************
 * functions to convert elements of abstract syntax tree to string
 *******************************************************************)

val string_of_sfp : sfp -> string

val string_of_type : _type -> string
