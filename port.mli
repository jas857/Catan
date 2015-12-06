open Utilities

(* Port module that contains the location of the port and
	its modified exchange rate. *)

type port = {
	location : coordinates;
	exchange : (int * int * int * int * int)
}
(* Trade rates for the different types of port. *)
val wildcard : (int * int * int * int * int)
val brick : (int * int * int * int * int)
val wool : (int * int * int * int * int)
val ore : (int * int * int * int * int)
val grain : (int * int * int * int * int)
val lumber : (int * int * int * int * int)

(* List of port locations hardcoded to match the original game. *)
val ports : port list
