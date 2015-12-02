(* open Gamestate *)
open ANSITerminal
open Utilities

type pixel = char * style list

type pixrow = pixel list

type pixboard = pixrow list

val print_pix : pixel -> unit

val print_row : pixrow -> unit

val print_pixboard : pixboard -> unit

(* Game module that contains the print and REPL functions.
	Communicates with the Gamestate to orchstrate the game. *)

(* Print out the hexagonal game board with legend, resources, and trade rates.*)
(* val print_board : gamestate -> unit *)

(* Print out the player's development cards given the color of the play *)
(* val print_dcards : color -> unit *)

(* Print out a list of commands and their functions depending on what the game_stage is *)
(* val print_help : stage -> unit *)

(* Main REPL function that takes input and does functions *)
(* val main_repl : gamestate -> gamestate *)
