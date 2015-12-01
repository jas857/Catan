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

let rec change_player_list (lst: player list) (plyr: player) : player list =
  match lst with
  | h::t -> if h.color = plyr.color then plyr::t else h::(change_player_list t plyr)
  | [] -> []

let get_resource (plyr: player) (resource: int) : int =
  match (resource, plyr.resources) with
  | 0, (x,_,_,_,_) -> x (* Brick *)
  | 1, (_,x,_,_,_) -> x (* Wool *)
  | 2, (_,_,x,_,_) -> x (* Ore *)
  | 3, (_,_,_,x,_) -> x (* Grain *)
  | 4, (_,_,_,_,x) -> x (* Lumber *)
  | _ -> failwith "Resource does not exist"

let change_resource (plyr: player) (resource: int) (amt: int) : player =
  match (resource, plyr.resources) with
  | 0, (_,x,y,z,w) -> {plyr with resources = (amt,x,y,z,w)}
  | 1, (x,_,y,z,w) -> {plyr with resources = (x,amt,y,z,w)}
  | 2, (x,y,_,z,w) -> {plyr with resources = (x,y,amt,z,w)}
  | 3, (x,y,z,_,w) -> {plyr with resources = (x,y,z,amt,w)}
  | 4, (x,y,z,w,_) -> {plyr with resources = (x,y,z,w,amt)}
  | _ , _          -> failwith "Change_resource parameters not met"

<<<<<<< HEAD
let rec update_largest_army (players: player list) (changing_player: player) =
  match players with
  | [] -> []
  | h::t -> if h.color = changing_player.color then
              changing_player::(update_largest_army t changing_player)
            else
              if h.largest_army then
                if changing_player.army_size > h.army_size then
                  let plyr = {h with victory_points = h.victory_points - 2;
                                     largest_army = false} in
                  plyr::t
                else
                  h::t
              else
                h::(update_largest_army t changing_player)
=======
let init_non_ai_player (c:color) = {roads_left = 15; roads = []; settlements_left = 5;
  cities_left = 4; towns = []; victory_points = 0;
  dcards = []; resources = (0,0,0,0,0); exchange = (4,4,4,4,4);
  color = c; a_i = false; army_size = 0; largest_army = false;
  road_size = 0; longest_road = false}
let initialize_non_ai_players () =
  [init_non_ai_player Red; init_non_ai_player Blue;
  init_non_ai_player White; init_non_ai_player Orange]
>>>>>>> bf640b7f8970aeb2854405dcc474ed85478cb292
