open Utilities
open Dcard
open Town

(* Player module that contains information regarding:
	-how many roads/settlements/cities the player has left
	-what roads/settlements/cities the player has on the board
	-the victory points the player has
	-the player's inventory
	-the exchange rate the player has
	-the player's color *)

type player = {
	roads_left : int;
	roads : (coordinates * coordinates) list;
	settlements_left : int;
	cities_left : int;
	towns : town list;
	victory_points : int;
	dcards : dcard list;
	resources : (int * int * int * int * int);
	exchange : (int * int * int * int * int);
	color : color;
	a_i : bool
}

