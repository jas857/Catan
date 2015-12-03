open Gamestate *)
open ANSITerminal
open Utilities
open Port
open Board
open Gamestate

type pixel = char * style list

type pixrow = pixel list

type pixboard = pixrow list

val grid_to_board : coordinates -> int*int

(* Update a row so that the given pixel list overwrites pixels starting
at the given index.*)
val write_str : pixrow -> int -> pixel list-> pixrow

(* Create a copy of the board where the input list is
written horizontally starting at x,y.*)
val write_board : pixboard -> (int * int) -> pixel list-> pixboard

(* Create a copy of the board where the input pixel overwrites pixels
from x,y to x+l,y *)
val write_block : pixboard -> (int * int) -> int -> pixel -> pixboard

(* Print a single character with ANSITerminal formatting *)
val print_pix : pixel -> unit

(* Print a list of pixels as if it were a string. *)
val print_row : pixrow -> unit

(* Print a list of lists of pixels. *)
val print_pixboard : pixboard -> unit

val print_port : pixboard -> port -> pixboard

(* Initialize a new printed board with tile info and grid.*)
val print_board : pixboard -> board -> pixboard

(* Create a copy of the board with the tile printed over it. *)
val print_tile : pixboard -> tile -> pixboard

(* Create a copy of the board with the player's resources and trade rates
printed in the correct place. *)
val print_resources : pixboard -> player -> pixboard

(* Create a copy of the board with the player's roads and settlements printed
over it. *)
val print_player : pixboard -> player -> pixboard

(* Print out the player's development cards given the color of the player*)
(* val print_dcards : color -> unit *)

(* Print out a list of commands and their functions depending on what the game_stage is *)
(* val print_help : stage -> unit *)

(* Main REPL function that takes input and does functions *)
(* val main_repl : gamestate -> unit *)