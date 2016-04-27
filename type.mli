type t = 
| Atom of string
| Arrow of (t * t)
| Var of string
| Apply of (t * t)
| Tuple of t list
| Union of (string * t) list

type declaration =
| NewType of (string * t option)
| Decl of (string * t) 
| Nothing

val atom : string -> t
val arrow : t -> t -> t
val var : string -> t
val tuple : t list -> t
val union : (string * t) list -> t
val _of : t -> t -> t

val compare : t -> t -> int

val output : out_channel -> t -> unit

val unify : t -> t -> (t * t) list

val map_variables : (t * t) list ->  t -> t

val apply : t -> t -> t option

val default_name : t -> string

val subtypes : t -> t list
val strict_subtypes : t -> t list
val union_subtypes : t -> t list

val expend : (string -> t option) -> t -> t

