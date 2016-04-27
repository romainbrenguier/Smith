module TSet : Set.S with type elt = Type.t
  
type t
val make : TSet.t -> Type.t -> t
val ( |- ) : TSet.t -> Type.t -> t
val empty : Type.t -> t
val ( ||- ) : unit -> Type.t -> t
val compare : t -> t -> int
val env : t -> TSet.t
val typ : t -> Type.t
val union : t -> t -> Type.t -> t
val set_type : t -> Type.t -> t
val set_env : t -> TSet.t -> t
val add_to_env : t -> Type.t -> t
val abstract : t -> Type.t -> t
val output : out_channel -> t -> unit
