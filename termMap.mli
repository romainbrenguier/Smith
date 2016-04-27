module EMap : Map.S with type key = Term.TSet.t

type 'a t 

val empty : 'a t
val add : Term.t -> 'a -> 'a t -> 'a t
val find : Term.t -> 'a t -> 'a
val find_typ : Type.t -> 'a t -> 'a EMap.t
val fold : (Term.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val iter : (Term.t -> 'a -> unit) -> 'a t -> unit
val merge : (Term.t -> 'a option -> 'a option -> 'a option) -> 'a t -> 'a t -> 'a t
