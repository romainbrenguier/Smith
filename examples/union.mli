type sum = I of int | S of string

val of_int : int -> sum

val to_string : sum -> (int -> string) -> string
