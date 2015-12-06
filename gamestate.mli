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
  playerturn : color; (* Color of the player whose turn it is *)
  players : player list; (* List of players *)
  game_board : board; (* Game Board *)
  game_stage : stage; (* Game Stage defined by stage above *)
  longest_road_claimed : bool; (* If Longest Road has been claimed *)
  largest_army_claimed : bool (* If largest army has been claimed *)
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
val prep_trade : gamestate -> string -> string -> int -> gamestate

(* Have the AI make a move *)
val a_i_makemove : gamestate -> gamestate

(* Change the player with the same color in the gamestate *)
val change_player : gamestate -> player -> gamestate

(* Finds the player in the list with the color input *)
val match_color : color -> player list -> player

(* Get input for coordinates for settlement *)
val get_settlement_info : unit -> coordinates

(* Checks if a settlement can be built at the coordinates *)
val can_build_settlement : gamestate -> coordinates -> bool

(* Builds a settlement and updates the gamestate *)
val build_settlement : gamestate -> coordinates -> bool -> gamestate

(* Checks to see if a road can be build at the two coordinates *)
val can_build_road : coordinates -> coordinates -> gamestate -> bool

(* Builds a road and updates the gamestate *)
val build_road : gamestate -> coordinates * coordinates -> gamestate

(* Gets input for coordinates where to build a road *)
val get_road_info : unit -> coordinates * coordinates

(* Adds amount of a certain resource to a player *)
val change_resource_for_distr : player -> environment -> int -> player

(* Distributes the resources for players with a certain environment *)
val dist_resources: player list -> (color*int) list -> environment-> player list

(* Collect resources for a players with that resource settlement *)
val collect_player_resource: player list -> tile list -> int -> player list

(* Returns the current player *)
val curr_player : gamestate -> player

(* Do the start stage things for AI *)
val ai_start_stage : gamestate -> gamestate

(* Play dcard or roll for AI *)
val ai_roll_or_play : gamestate -> gamestate

(* Play dcard or build things for AI *)
val ai_build_or_play : gamestate -> gamestate
