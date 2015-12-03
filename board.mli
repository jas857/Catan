open Tile
open Port
open Dcard

(* Board module that contains a list of tiles, a list of ports, and
	a list of dcards. *)

type board = {
	tiles : tile list;
	ports : port list;
	dcards : dcard list;
    blocks : bool array array
}

(* Generates a new Board to be used by randomly placing tiles, and randomly
creating a dcard list, port locations should be fixed *)
val initialize_board : unit -> board

val can_build : int -> int -> board -> bool
