open Tile
open Port
open Dcard

(* Board module that contains a list of tiles, a list of ports, and
	a list of dcards. *)

type board = {
	tiles : tile list;
	ports : port list;
	dcards : dcard list
}

(* Generates a new Board to be used by randomly placing tiles, and randomly
creating a dcard list, port locations should be fixed *)
val initialize_board : unit -> board
(* Generates the randomly placed tile list that makes up the board*)
val initialize_tiles : unit -> tile list
(* Generates the stack fo dcards in a random order *)
val initialize_dcards : unit -> dcard list