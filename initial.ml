open Utilities
open Board
open Gamestate
open Player
open Town
open Tile

(* Rebuild the int*color representation of towns on the tiles. Used to make
the board reflect the players' hardcoded towns in fast_gs. *)
let rebuild_towns gs =
  let rebuild (tl:tile) (pl:player) =
    let towns = List.filter (fun t -> List.mem t.location (corners tl)) pl.towns in
    let num = List.fold_left (+) 0 (List.map (fun t -> t.pickup) towns) in
    {tl with towns=(pl.color,num)::tl.towns} in
  {gs with game_board=
    {gs.game_board with tiles=
      List.map (fun t -> List.fold_left rebuild {t with towns=[]} gs.players)
        gs.game_board.tiles}}




let red_player = {
  roads_left=15;
  (* roads=[((2,2),(3,3));((3,3),(4,3));((4,3),(5,3))]; *)
  roads=[];
  settlements_left=5;
  cities_left=4;
  (* towns=[{location=(2,2);pickup=1};{location=(7,3);pickup=2}]; *)
  towns=[];
  victory_points=0;
  dcards=[];
  resources = (0,0,0,0,0);
  exchange = (4,4,4,4,4);
  color = Red;
  a_i = false;
  ai_vars = {curpos= (0,0); left=0;right=0;up=0;down=0};
  army_size=0;
  largest_army=false;
  road_size=0;
  longest_road = false
}
let blue_player = {red_player with color=Blue}
let white_player = {red_player with color=White}
let orange_player = {red_player with color=Orange}

let default_gs = {playerturn=Red;
               players=[red_player;blue_player;white_player;orange_player];
               game_board=initialize_board ();
               game_stage=Start;
               longest_road_claimed=false;
               largest_army_claimed=false}

let single_player_gs = {playerturn=Red;
               players = initialize_single_player ();
               game_board=initialize_board ();
               game_stage=Start;
               longest_road_claimed=false;
               largest_army_claimed=false}

let trade_gs =
  {default_gs with
  players=List.map (fun p -> {p with resources=(9,9,9,9,9)}) default_gs.players;
  game_stage=Build}

let fast_gs = rebuild_towns {default_gs with
                             game_stage=Production;
                             players=[
                                {red_player with
                                 towns=[twn (0,0) 1; twn (2,0) 1];
                                 roads=[((0,0),(1,1));((1,1),(2,1))];
                                 resources=(6,6,6,6,6);
                                 roads_left=13;
                                 settlements_left=3};
                                 {blue_player with
                                 towns=[twn (4,0) 1; twn (0,1) 1];
                                 roads=[((4,0),(5,0));((0,1),(1,1))];
                                 resources=(6,6,6,6,6);
                                 roads_left=13;
                                 settlements_left=3};
                                 {white_player with
                                 towns=[twn (2,1) 1; twn (4,1) 1];
                                 roads=[((2,1),(3,1));((3,1),(4,1))];
                                 resources=(6,6,6,6,6);
                                 roads_left=13;
                                 settlements_left=3};
                                 {orange_player with
                                 towns=[twn (6,1) 1; twn (0,2) 1];
                                 roads=[((6,1),(7,1));((7,1),(6,0))];
                                 resources=(6,6,6,6,6);
                                 roads_left=13;
                                 settlements_left=3}]}
                                 
let complete_gs = {fast_gs with game_stage = End}
