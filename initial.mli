open Utilities
open Board
open Gamestate
open Player
open Town

(* This file contains a number of gamestates we used to quickly launch
into and test different game stages. *)

(* A gamestate which starts off with lots of resources and nothing
on the board, in Trade stage. *)
val trade_gs : gamestate

(* A gamestate with "normal" initial conditions, for playing a standard
game. Begins in Build. *)
val default_gs : gamestate

(* A gamestate with 3 ai players *)
val single_player_gs : gamestate

(* A normal gamestate where Build has already been completed. *)
val fast_gs : gamestate

(*A completed gamestate*)
val complete_gs : gamestate
