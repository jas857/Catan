open Utilities
open Dcard
open Town

(* Mutable variables used for the search algorithms which back
our AI. *)
type a_i_variables = {
  mutable curpos : coordinates;
  mutable left : int;
  mutable right : int;
  mutable up : int;
  mutable down : int
}
(* Stores all specific information about a single player.
The only duplication of this data is in tiles, where the
towns are represented again to make resource distribution
easier.*)
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
  ai_vars : a_i_variables;
  army_size : int;
  largest_army : bool;
  road_size : int;
  longest_road : bool
}

(* Automatically finds the old version of the provided player based on
color and replacees them. *)
val change_player_list : player list -> player -> player list
(* Get a specified resource from a player *)
val get_resource : player -> int -> int
(* Get a specified trade exchange rate from a player *)
val get_exchange : player -> int -> int
(* Change the specified resource by amt. *)
val change_resource  : player -> int -> int -> player
(* Change multiple resources at once with a 5-tuple representing the overall
   change. *)
val change_resources : player -> rsrc -> player
(* Update the owner of the largest army point bonus. *)
val update_largest_army : player list -> player -> player list
(* Initialize all four players as humans. *)
val initialize_non_ai_players : unit -> player list
(* Initialize a computer player. *)
val init_ai_player : color -> player
(* Initialize 3 ai and 1 human player. *)
val initialize_single_player : unit -> player list
(* Initialize a human player *)
val init_non_ai_player : color -> player
(* Initialize the stated number of ai players. *)
val initialize_ai_players : int -> player list
(* Checks whether a specified coordinate pair has a road on it. *)
val is_road : (coordinates * coordinates) -> player list -> bool
(* Check whether a given corner has an unbuilt road edge touching it. *)
val corner_can_expand : coordinates -> player list -> bool
(* Move the search cursor for a given player's ai. *)
val curpos_change :
    (coordinates * coordinates) list -> player -> player list-> (bool* player)
