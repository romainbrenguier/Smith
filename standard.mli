external compare : 'a -> 'a -> int = "%compare"
val min : 'a -> 'a -> 'a
val ( = ) : 'a -> 'a -> bool 
external not : bool -> bool = "%boolnot"
external ( && ) : bool -> bool -> bool = "%sequand"
external succ : int -> int = "%succint"
external ( + ) : int -> int -> int = "%addint"
val max_int : int
external ( +. ) : float -> float -> float = "%addfloat"
external sqrt : float -> float = "caml_sqrt_float"
val infinity : float
val ( ^ ) : string -> string -> string
external int_of_char : char -> int = "%identity"
val char_of_int : int -> char
external ignore : 'a -> unit = "%ignore"
val string_of_bool : bool -> string
