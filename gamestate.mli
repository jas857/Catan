open Player
open Game
open Board

(* Gamestate module. Holds information and communicates with all modules. *)

(* XY coodinates of the corners of hexagons *)
type coordinates = (int * int)

(* Color of player *)
type color =
	| Red | Blue | White | Orange

(* Stage of the game *)
type stage =
	| Start | Production | Trade | Build | End

(* State of the game *)
type gamestate = {
	playerturn : color;
	players : player list;
	game_board : board;
	game_stage : stage
}

(* Change the turn to a different player *)
val change_turn : gamestate -> gamestate

(* Change the stage of the game *)
val change_stage : gamestate -> gamestate

(* Find the tile corresponding to coordinates *)
val find_tile : gamestate -> coordinates -> tile