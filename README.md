# Smith
Synthesis of ML Inferred from Type Headers


## Purpose

This tool automatically generates OCaml program from headers (.mli files).
It suggests a function by searching for the simplest program that has the given type.
Examples are given in the <examples> directory.
For now, arrays, sum and pairs are supported.
 

## Usage 

To try Smith run the following example:
> ./smith examples/tuple.mli -o examples/tuple.ml

This should generate a program in tuple.ml for the types given here:

> type tuple = int * string * int
> 
> val make : int -> string -> tuple
> val get_int : (int * string) -> int



