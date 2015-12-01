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
  a_i : bool;
  army_size : int;
  largest_army : bool;
  road_size : int;
  longest_road : bool
}


val change_player_list : player list -> player -> player list
val get_resource : player -> int -> int
val change_resource : player -> int -> int -> player
<<<<<<< HEAD
val update_largest_army : player list -> player -> player list
=======
val initialize_non_ai_players : unit -> player list
val init_non_ai_player : color -> player
>>>>>>> bf640b7f8970aeb2854405dcc474ed85478cb292
