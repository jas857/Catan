open Gamestate

(* Tile module that contains information regarding the state of each tile *)

(* Name of environment. Determined what resource is picked up. *)
type environment =
	| Hills | Pasture | Mountains | Fields | Forest | Desert

(* Tile location is defined by a char A-S and labeled on the board *)
type tile_location = char

type tile = {
	env : environment;
	collect_on : int;
	loc : tile_location;
	corner : coordinates;
	towns : (color * int) list;
	robber : bool
}

(* Add town to list of towns in tile *)
val add_town : gamestate -> gamestate