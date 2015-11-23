open Tile 
open Port 
open Dcard

(* Board module that contains a list of tiles, a list of ports, and
	a list of dcards. *)

type board = {
	tiles : tile list;
	ports : port list;
	dcards : dcard list
}

(* Move the robber among tiles *)
val move_robber : gamestate -> gamestate

(* Pick a card out of the list of cards and remove it *)
val pick_card : gamestate -> gamestate