open Player
open Board
open Utilities
open Dcard
open Tile
open Town

(* Gamestate module. Holds information and communicates with all modules. *)

type stage =
  | Start | Production | Trade | Build | End

(* State of the game *)
type gamestate = {
  playerturn : color;
  players : player list;
  game_board : board;
  game_stage : stage;
  longest_road_claimed : bool;
  largest_army_claimed : bool
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


(* returns tile given a character location of the tile *)
let rec get_tile (tiles:tile list) s =
  match tiles with
  |h::t -> if h.loc = (Char.uppercase s) then Some h else get_tile t s
  |_ -> None

(* adds a new town tuple to a tile town list *)
let add_town (gs:gamestate) (t:tile) (ci:color*int) : gamestate =
  let newT = {t with towns = ci::(t.towns)} in
  let temp_board = gs.game_board in
  let new_board =
  {temp_board with tiles = rebuild_tile_list temp_board.tiles newT} in
  {gs with game_board = new_board}


(* removes robber from current location, and then places robber in new
location*)
let rec move_robber (gs: gamestate) : gamestate =
  let _ = print_string
  "Please enter the letter of the tile you would like to move the robber to: " in
  let input = read_line() in
  if (Bytes.length input) > 1
  then (let _ = print_endline "Invalid tile location" in move_robber gs)
  else
  let newRobLoc = get_tile (gs.game_board).tiles (Bytes.get input 0) in
  match newRobLoc with
  |None -> let _ = print_endline "Invalid tile location" in move_robber gs
  |Some loc ->
    let nrl = loc in
    let gboard = gs.game_board in
    let robberless =
    rebuild_tile_list gboard.tiles (remove_robber gboard.tiles) in
    let newBoard =
    {gboard with tiles = rebuild_tile_list robberless nrl} in
    {gs with game_board = newBoard}



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
  let plyr = match_color (state.playerturn) (state.players) in
  let plyr = change_resource plyr resource1
        ((get_resource plyr resource1) + 1) in
  let plyr = change_resource plyr resource2
        ((get_resource plyr resource2) + 1) in
  change_player state plyr

let play_road_building () = failwith "TODO" (* Call Road Building Method(s) *)

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

let check_largest_army (state: gamestate) (changing_player: player): gamestate =
  let new_players = update_largest_army state.players changing_player in
  if state.players = new_players then
    if state.largest_army_claimed = false then
    {state with players =
                        (change_player_list state.players
                          {changing_player with
                            victory_points = changing_player.victory_points + 2;
                            largest_army = true});
                largest_army_claimed = true}
    else change_player state changing_player
  else {state with players = new_players}

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

let check_largest_army (state: gamestate) (changing_player: player): gamestate =
  let new_players = update_largest_army state.players changing_player in
  if state.players = new_players then
    if state.largest_army_claimed = false then
    {state with players =
                        (change_player_list state.players
                          {changing_player with
                            victory_points = changing_player.victory_points + 2;
                            largest_army = true});
                largest_army_claimed = true}
    else change_player state changing_player
  else {state with players = new_players}

let play_dcard (state: gamestate) (card: dcard) : gamestate =
  let player = match_color (state.playerturn) (state.players) in
  if List.mem card player.dcards then
  let player = {player with dcards = (remove_from_list player.dcards card)} in
  (match card with
  | Knight -> move_robber
      (check_largest_army state {player with army_size = player.army_size + 1})
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
                else
                  play_year_plenty state ((num -(num mod 10))/10) (num mod 10)
               | Road_Building -> failwith "TODO"(* Do road building action *)))
  else state

let rec search_list (coor:coordinates) (tcoorList): bool =
  match tcoorList with
  | [] -> false
  | (x,y)::t -> if ((x = coor) || (y = coor)) then true
                else search_list coor t

let rec search_towns (coor: coordinates) (towns: town list): bool =
  match towns with
  | [] -> false
  | h::t -> if (coor = h.location)
              then true
            else search_towns coor t


(*check whether or not it is possible to build a road where specified*)
let is_valid_build_road (coor: coordinates) (p : player): bool =
  (search_list coor p.roads) || (search_towns coor p.towns)

let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

(*modifies the gamestate to include the built road*)
let rec build_road (state: gamestate): gamestate =
  let _ = print_string
  "Please enter the letter of the tile you would like to start your road on: " in
  let start_tile = read_line() in
  if(String.length start_tile <> 1) then let _ =
  print_string "unacceptable input" in build_road state
  else let s_tile = start_tile.[0] in


  let _ = print_string "Please enter the number of the tile
  corner you would like to start your road on: " in
  let start_corner = read_line() in
  if(not (is_int start_corner)) then let _ =
  print_string "unacceptable input" in build_road state
  else let s_corner = int_of_string start_corner in


  let _ = print_string
  "Please enter the letter of the tile you would like to start your road on: " in
  let end_tile = read_line() in
  if(String.length start_tile <> 1) then let _ =
  print_string "unacceptable input" in build_road state
  else let e_tile = end_tile.[0] in


  let _ = print_string "Please enter the number of the tile corner you
  would like to start your road on: " in
  let end_corner = read_line() in
  if(not (is_int end_corner)) then let _ =
  print_string "unacceptable input" in build_road state
  else let e_corner = int_of_string end_corner in



  let startTileCoor = (conv s_tile s_corner) in
        let endTileCoor = (conv e_tile e_corner) in
        let currentPlayer = find_player state.playerturn state.players in
        if((is_valid_build_road startTileCoor currentPlayer)
          || (is_valid_build_road endTileCoor currentPlayer))
        then
        let updatePlayer = {currentPlayer with roads =
        (((startTileCoor),(endTileCoor))::(currentPlayer.roads))} in
        let updatePlayerAgain = {updatePlayer with
        roads_left = (updatePlayer.roads_left - 1)} in
        let newPlayerList = change_player_list state.players updatePlayerAgain in
        {state with players = newPlayerList}


  else let _ = print_string "The inputs you have entered
   are not valid. Please try again." in build_road state
let rec build (state: gamestate) (input:string): gamestate =
  let player = match_color (state.playerturn) (state.players) in
  (match String.lowercase input with
  |"road" -> if (get_resource player 0) > 0 && (get_resource player 4) > 0
          then failwith "TODO"(* BUILD ROAD *)
          else print_endline ("Insufficient resources"); state
  |"settlement" -> if (get_resource player 0) > 0 &&
             (get_resource player 1) > 0 &&
             (get_resource player 3) > 0 &&
             (get_resource player 4) > 0
             then failwith "TODO"(* BUILD SETTLEMENT *)
             else print_endline ("Insufficient resources"); state
  |"city" -> if (get_resource player 3) > 1 &&
           (get_resource player 2) > 2
            then failwith "TODO"(* BUILD CITY *)
            else print_endline ("Insufficient resources"); state
  |"dcard" -> if (get_resource player 1) > 0 &&
           (get_resource player 2) > 0 &&
           (get_resource player 3) > 0
            then failwith "TODO"(* BUILD DCARD *)
            else print_endline ("Insufficient resources"); state
  | _ -> build state input)


let pick_dcard gs = failwith "TODO"

let a_i_makemove gs = failwith "TODO"

let trade gs = failwith "TODO"
