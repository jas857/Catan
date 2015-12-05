open Player
open Board
open Utilities
open Tile
open Dcard

(* Gamestate module. Holds information and communicates with all modules. *)

(* Stage of the game *)
type stage =
	| Start | Production | Build | End

(* State of the game *)
type gamestate = {
  playerturn : color;
  players : player list;
  game_board : board;
  game_stage : stage;
  longest_road_claimed : bool;
  largest_army_claimed : bool
}

val get_tile : tile list -> char -> tile option
(* Change the turn to a different player *)
val change_turn : gamestate -> gamestate

(* Change the stage of the game *)
val change_stage : gamestate -> gamestate

(* Add a town to the list of towns on a tile *)
val add_town : gamestate -> tile -> (color*int) -> gamestate
(* play a given dcard in the current gamestate*)
val play_dcard : gamestate -> dcard -> gamestate
(* Move the robber among tiles *)
val move_robber : gamestate ->  tile_location -> gamestate

(* Pick a card out of the list of cards and remove it *)
val pick_dcard : gamestate -> gamestate
(* Build a road, settlement, city, or dcard *)
val build : gamestate -> string -> gamestate

(* Trade among players/bank *)
val trade : gamestate -> string -> string -> int -> gamestate

(* Have the AI make a move *)
val a_i_makemove : gamestate -> gamestate

val change_player : gamestate -> player -> gamestate

val match_color : color -> player list -> player

val get_settlement_info : unit -> coordinates

val can_build_settlement : gamestate -> coordinates -> bool

val build_settlement : gamestate -> coordinates -> bool -> gamestate

val can_build_road : coordinates -> coordinates -> gamestate -> bool

val build_road : gamestate -> coordinates * coordinates -> gamestate

val get_road_info : unit -> coordinates * coordinates

val change_resource_for_distr : player -> environment -> int -> player

val dist_resources: player list -> (color*int) list -> environment-> player list

val collect_player_resource: player list -> tile list -> int -> player list

val curr_player : gamestate -> player

val ai_start_stage : gamestate -> gamestate

val ai_roll_or_play : gamestate -> gamestate

val ai_build_or_play : gamestate -> gamestate
