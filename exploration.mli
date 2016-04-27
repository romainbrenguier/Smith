module TSet : Set.S with type elt = Type.t
module ESet : Set.S with type elt = Term.t
(*module EMap : Map.S with type key = Term.t*)

val map_to_set : 'a TermMap.t -> ESet.t
val intro : Program.t TermMap.t -> Type.t list -> Program.t TermMap.t
val apply : Program.t TermMap.t -> Program.t TermMap.t -> Program.t TermMap.t
val exploration_step : Program.t TermMap.t -> Type.t -> Program.t TermMap.t

