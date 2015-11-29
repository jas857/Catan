open Gamestate


(* Game module that contains the print and REPL functions.
	Communicates with the Gamestate to orchstrate the game. *)

(* Print out the hexagonal game board *)
val print_board : gamestate -> unit

(* Print out the player's resources given the color of the player *)
val print_resources : color -> unit

(* Print out the player's development cards given the color of the play *)
val print_dcards : color -> unit

(* Print out a list of commands and their functions depending on what the game_stage is *)
val print_help : stage -> unit

(* Main REPL function that takes input and does functions *)
val main_repl : gamestate -> unit