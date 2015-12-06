open Tile
open Port
open Dcard

(* Board module that represents all the player-independent state
of the board. The tiles also replicate some state from the players
to remember which players receive resources when their collect_on
number is rolled.*)
type board = {
	tiles : tile list; (* A list of tiles on the board *)
	ports : port list; (* A list of ports on the board *)
	dcards : dcard list; (* A list of development cards available *)
}

(* Generates a new Board to be used by randomly placing tiles, and randomly
creating a dcard list, port locations should be fixed *)
val initialize_board : unit -> board
