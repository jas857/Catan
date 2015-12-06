open Tile
open Port
open Dcard

(* Board module that contains a list of tiles, a list of ports, and
	a list of dcards. *)

type board = {
	tiles : tile list; (* A list of tiles on the board *)
	ports : port list; (* A list of ports on the board *)
	dcards : dcard list; (* A list of development cards available *)
}

(* Generates a new Board to be used by randomly placing tiles, and randomly
creating a dcard list, port locations should be fixed *)
val initialize_board : unit -> board
