open Utilities

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

(* replace a tile  in the tile with it's updated tile *)
val rebuild_tile_list : tile list -> tile -> tile list

(* removes the robber from the tile that has it, and returns the tile*)
val remove_robber : tile list -> tile
(* Generates the randomly placed tile list that makes up the board*)
val initialize_tiles : unit -> tile list