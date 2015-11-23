open Gamestate

(* Port module that contains the location of the port and
	its modified exchange rate. *)

type port = {
	location : coordinates;
	exchange : (int * int * int * int * int)
}