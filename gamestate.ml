open Player
open Board
open Utilities
open Dcard
open Tile

(* Gamestate module. Holds information and communicates with all modules. *)

type stage =
  | Start | Production | Trade | Build | End

(* State of the game *)
type gamestate = {
  playerturn : color;
  players : player list;
  game_board : board;
  game_stage : stage
}


(* returns the gamestate with the next player's turn if going forward,
can be used for finding next player turn in start stage and play stage *)
let next_forward (gs:gamestate) =
  let c = gs.playerturn in
  match c with
  |Red -> {gs with playerturn = Blue}
  |Blue -> {gs with playerturn = White}
  |White -> {gs with playerturn = Orange}
  |Orange -> if gs.game_stage = Start
             then gs
             else {gs with playerturn = Red}

  (* returns the gamestate with next player's turn if going backward
  used only for start stage *)
let next_backward (gs:gamestate) =
  let c = gs.playerturn in
  match c with
  |Red -> {gs with game_stage = Production}
  |Blue -> {gs with playerturn = Red}
  |White -> {gs with playerturn = Blue}
  |Orange -> {gs with playerturn = White}

(*picks out the player with a corresponding color *)
let rec match_color (c:color) (pl:player list) =
  match pl with
  |[] -> failwith "No player with that color"
  | h::t -> if h.color = c then h else match_color c t

(*returns a gamestate with the new players turn during the start stage *)
let choose_next_start (gs:gamestate) =
  let a = match_color (gs.playerturn) (gs.players) in
  let ar = a.roads_left in
  let asl = a.settlements_left in
  if (ar = 14 && asl = 4) then
  next_forward gs
  else
  if (ar = 13 && asl = 3) then
  next_backward gs
  else
  failwith "Change_stage called at incorrect time"

  (* Change the turn to the next player *)
let change_turn (gs:gamestate) =
  next_forward gs
  (*print out victor and any other details about game over*)
let game_complete (gs:gamestate) =
  failwith "todo"
(* Change the stage of the game, should only be called when a player has
has finsihed a stage and the stage should be changed *)
let change_stage (gs:gamestate) =
  match gs.game_stage with
  |Start -> choose_next_start gs
  |Production -> {gs with game_stage = Trade}
  |Trade -> {gs with game_stage = Build}
  |Build -> change_turn gs
  |End -> game_complete gs

(* Find the tile corresponding to coordinates *)
let find_tile  (gs:gamestate)  (c:coordinates)  (t:tile) =
  failwith "todo"

let add_town (gs:gamestate) (t:tile) (ci:color*int) =
  let newT = {t with towns = ci::(t.towns)} in
  let temp_board = gs.game_board in
  let new_board = {temp_board with tiles = rebuild_tile_list temp_board.tiles t} in
  {gs with game_board = new_board}

let move_robber (state: gamestate) : gamestate = failwith "TODO"

let rec change_player (state: gamestate) (plyr: player) : gamestate =
  let lst = change_player_list (state.players) (plyr) in
  {state with players = lst}

let play_monopoly (state: gamestate) (resource: int) : gamestate =
  let toAdd = List.fold_left (fun acc x ->
                  if x.color = state.playerturn
                  then acc
                  else acc + (get_resource x resource))
              0 state.players in
  let plyrs = List.map (fun x ->
              if x.color = state.playerturn
              then change_resource x resource
                (toAdd +(get_resource x resource))
              else change_resource x resource 0) (state.players) in
  {state with players = plyrs}

let play_year_plenty
  (state: gamestate) (resource1: int) (resource2: int) : gamestate =
  let plyr = find_player (state.playerturn) (state.players) in
  let plyr = change_resource plyr resource1
        ((get_resource plyr resource1) + 1) in
  let plyr = change_resource plyr resource2
        ((get_resource plyr resource2) + 1) in
  change_player state plyr

let play_road_building = failwith "TODO" (* Call Road Building Method(s) *)

(* Dcard *)
  let play_card (state: gamestate) (card: dcard) : gamestate =
  let player = find_player (state.playerturn) (state.players) in
  if List.mem card player.dcards then
  let player = {player with dcards = (remove_from_list player.dcards card)} in
  (match card with
  | Knight -> move_robber state
  | Victory_Card (name, desc) ->
       (print_endline ("You played: " ^ name ^ "- " ^ desc));
       let plyr = {player with victory_points = player.victory_points + 1} in
       if (plyr.victory_points = 10) then {state with game_stage = End} else
       change_player (state) (plyr)
  | Progress_Card p -> (match p with
               | Monopoly ->
          let num = get_input true
          ("Input one type of resource to take from all opponents\n
          (0 = Brick, 1 = Wool, 2 = Ore, 3 = Grain, 4 = Lumber):") in
          play_monopoly state num
               | Year_of_plenty ->
          let num = get_input false
          ("Input two types of resources to take with no space\n
          (0 = Brick, 1 = Wool, 2 = Ore, 3 = Grain, 4 = Lumber):") in
          if num < 10 then play_year_plenty state 0 num
          else play_year_plenty state ((num -(num mod 10))/10) (num mod 10)
               | Road_Building -> failwith "TODO"(* Do road building action *)))
  else state