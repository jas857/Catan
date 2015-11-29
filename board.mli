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


