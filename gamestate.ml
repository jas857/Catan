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

  (* Change the turn to the next player during play phase *)
let change_turn (gs:gamestate) =
  {(next_forward gs) with game_stage = Production}

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
  let newT = {t with towns = (ci::(t.towns))} in
  let temp_board = gs.game_board in
  let new_board =
  {temp_board with tiles = rebuild_tile_list temp_board.tiles newT} in
  {gs with game_board = new_board}


(* removes robber from current location, and then places robber in new
location given by the input string*)
let rec move_robber (gs: gamestate) (c:tile_location): gamestate =
  (* let _ = print_string
  "Please enter the letter of the tile you would like to move the robber to: " in
  let input = read_line() in
  if (Bytes.length input) > 1
  then (let _ = print_endline "Invalid tile location" in move_robber gs )
  else *)
  let newRobLoc = get_tile (gs.game_board).tiles c in
  match newRobLoc with
  |None -> let _ = print_endline "Invalid tile location"  in gs
  |Some loc ->
    let nrl = {loc with robber =true} in
    let gboard = gs.game_board in
    let robberless =
    rebuild_tile_list gboard.tiles (remove_robber gboard.tiles) in
    let newBoard =
    {gboard with tiles = rebuild_tile_list robberless nrl} in
    {gs with game_board = newBoard}



let rec change_player (state: gamestate) (plyr: player) : gamestate =
  let lst = change_player_list (state.players) (plyr) in
  {state with players = lst}


let check_largest_army (state: gamestate) (changing_player: player): gamestate =
  let new_players = update_largest_army state.players changing_player in
  if state.players = new_players then
    if state.largest_army_claimed = false then
      if changing_player.army_size >= 3 then
    {state with players =
                        (change_player_list state.players
                          {changing_player with
                            victory_points = changing_player.victory_points + 2;
                            largest_army = true});
                largest_army_claimed = true}
      else change_player state changing_player
    else change_player state changing_player
  else {state with players = new_players}

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

let rec is_overlap_road (road: coordinates * coordinates)
(players: player list) : bool =
  match players with
  | [] -> false
  | h::t -> let (a,b) = road in
        if ((List.mem road h.roads) || (List.mem (b,a) h.roads))
        then true
        else is_overlap_road road t

(*says if road can be built at given coordinates*)
let can_build_road (startTileCoor: coordinates)
(endTileCoor: coordinates) (state: gamestate) =
  let currentPlayer = match_color state.playerturn state.players in
  (((is_valid_build_road startTileCoor currentPlayer)
          || (is_valid_build_road endTileCoor currentPlayer))
        && not (is_overlap_road (startTileCoor, endTileCoor) state.players))

let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

(*modifies the gamestate to include the built road*)

let rec get_road_info () :(coordinates * coordinates) =
  let _ = print_string
  "Please enter the letter of the tile you would like to start your road on: " in
  let start_tile = read_line() in
  if(String.length start_tile <> 1) then let _ =
  print_string "unacceptable input" in get_road_info ()
  else let s_tile = start_tile.[0] in

  let _ = print_string "Please enter the number of the tile
  corner you would like to start your road on: " in
  let start_corner = read_line() in
  if(not (is_int start_corner)) then let _ =
  print_string "unacceptable input" in get_road_info ()
  else let s_corner = int_of_string start_corner in

  let _ = print_string
  "Please enter the letter of the tile you would like to end your road on: " in
  let end_tile = read_line() in
  if(String.length start_tile <> 1) then let _ =
  print_string "unacceptable input" in get_road_info ()
  else let e_tile = end_tile.[0] in

  let _ = print_string "Please enter the number of the tile corner you
  would like to end your road on: " in
  let end_corner = read_line() in
  if(not (is_int end_corner)) then let _ =
  print_string "unacceptable input" in get_road_info ()
  else let e_corner = int_of_string end_corner in

  let startTileCoor = (conv s_tile s_corner) in
  let endTileCoor = (conv e_tile e_corner) in
  (startTileCoor, endTileCoor)

(*builds the actual road in game state*)
let rec build_road (state: gamestate)
(coor: (coordinates * coordinates)): gamestate =
  let (startTileCoor, endTileCoor) = coor in
  let currentPlayer = match_color state.playerturn state.players in
  let updatePlayer = {currentPlayer with roads =
  (((startTileCoor),(endTileCoor))::(currentPlayer.roads))} in
  let updatePlayerAgain = {updatePlayer with
  roads_left = (updatePlayer.roads_left - 1)} in
  let newPlayerList = change_player_list state.players updatePlayerAgain in
  {state with players = newPlayerList}


(*if the tile contains the town then add to the towns list of the tile*)
let rec settlement_helper (tiles: tile list)
 (coor: coordinates) (clr: color) : tile list =
  match tiles with
  | [] -> []
  | h::t -> if(((conv h.loc 0) = coor) ||
    ((conv h.loc 1) = coor) || ((conv h.loc 2) = coor) ||
    ((conv h.loc 3) = coor) || ((conv h.loc 4) = coor) ||
    ((conv h.loc 5) = coor))
  then {h with towns = (clr, 1)::(h.towns)}::(settlement_helper t coor clr)
  else h::(settlement_helper t coor clr)

let rec get_settlement_info () : coordinates =
  let _ = print_string
  "Please enter the letter of the tile
  you would like to build your settlement on: " in
  let start_tile = read_line() in
  if(String.length start_tile <> 1) then let _ =
  print_string "unacceptable input" in get_settlement_info ()
  else let s_tile = start_tile.[0] in

  let _ = print_string "Please enter the number of the tile
  corner you would like to build your settlement on: " in
  let start_corner = read_line() in
  if(not (is_int start_corner)) then let _ =
  print_string "unacceptable input" in get_settlement_info ()
  else let s_corner = int_of_string start_corner in
  (conv s_tile s_corner)

let can_build_settlement (gs: gamestate) (coor: coordinates): bool =
  let adjs = adjacents coor in
  let blocks_build (pl:player) =
    any (fun t -> List.mem t.location adjs) pl.towns in
  not (any blocks_build gs.players)




let rec build_settlement (gs: gamestate) ( coor: coordinates): gamestate =
  let currentPlayer = match_color gs.playerturn gs.players in
  let updatedPlayer = {currentPlayer with
  settlements_left = currentPlayer.settlements_left - 1;
  towns = {location = coor; pickup = 1}::(currentPlayer.towns)} in
  let newPlayerList = change_player_list gs.players updatedPlayer in
  let updatedBoard =
  {gs.game_board with tiles =
  settlement_helper gs.game_board.tiles coor currentPlayer.color} in
  {gs with players = newPlayerList; game_board = updatedBoard}

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

let play_road_building (state:gamestate) (r1:(coordinates *coordinates))
(r2:(coordinates * coordinates)): gamestate =
  let state = build_road state r1 in
  build_road state r2

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


let play_dcard (state: gamestate) (card: dcard) : gamestate =
  let player = match_color (state.playerturn) (state.players) in
  if List.mem card player.dcards then
  let player = {player with dcards = (remove_from_list player.dcards card)} in
  (match card with
  | Knight -> let _ = print_string
  "Please enter the letter of the tile you would like to move the robber to: " in
  let input = read_line() in
  if (Bytes.length input) > 1
  then (let _ = print_endline "Invalid tile location" in state )
  else move_robber
      (check_largest_army state {player with army_size = player.army_size + 1})
      (Bytes.get input 0)
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
               | Road_Building -> let r1 = get_road_info () in
                                  let r2 = get_road_info () in
                                  play_road_building state r1 r2))
else let _ = print_endline "You do not have that dcard" in state

let pick_dcard gs =
  let tempPlayer = match_color gs.playerturn gs.players in
  let temp = change_player_list gs.players
  {tempPlayer with dcards =
  (List.hd gs.game_board.dcards)::(tempPlayer.dcards)} in
  {gs with players = temp;game_board =
  { gs.game_board with dcards = List.tl gs.game_board.dcards}}

let trade gs = failwith "TODO"

let rec loop_tiles (tiles: tile list) (plyr: player) : unit =
    match tiles with
    | [] -> ()
    | h::t -> if List.length h.towns = 3 then ()
               else
                (if h.collect_on <= 4 || h.collect_on >= 10 then ()
                    else
                      if ((fst h.corner) < (fst plyr.ai_vars.curpos)) then
                        if ((snd h.corner) < (snd plyr.ai_vars.curpos)) then
                          let _ =(plyr.ai_vars.left <- plyr.ai_vars.left + 1) in
                          let _ =(plyr.ai_vars.down <- plyr.ai_vars.down + 1) in
                          loop_tiles t plyr
                        else
                          let _ =(plyr.ai_vars.left <- plyr.ai_vars.left + 1) in
                          let _ =(plyr.ai_vars.up <- plyr.ai_vars.up + 1) in
                          loop_tiles t plyr
                      else
                        if ((snd h.corner) < (snd plyr.ai_vars.curpos)) then
                          let _ =(plyr.ai_vars.right <- plyr.ai_vars.right + 1) in
                          let _ =(plyr.ai_vars.down <- plyr.ai_vars.down + 1) in
                          loop_tiles t plyr
                        else
                          let _ =(plyr.ai_vars.right <- plyr.ai_vars.right + 1) in
                          let _ =(plyr.ai_vars.up <- plyr.ai_vars.up + 1) in
                          loop_tiles t plyr)


(* AI Functions *)
let ai_update_directions (state: gamestate) (plyr: player) : unit =
  (plyr.ai_vars.left <- 0);
  (plyr.ai_vars.right <- 0);
  (plyr.ai_vars.up <- 0);
  (plyr.ai_vars.down <- 0);
  loop_tiles state.game_board.tiles plyr

let can_move (state: gamestate) (coord: coordinates): bool =
  (can_build_road (match_color state.playerturn state.players).ai_vars.curpos coord state)
  && (*can_build coord &&*) not(List.mem coord oob)
(* Checks if the coordinate has a road coming from curpos into it. Also checks if there is a town there. Also checks if it is off the board *)

(* AI move position to build road *)
let rec move_position (state: gamestate) (plyr: player)  : gamestate =
  let start = plyr.ai_vars.curpos in
  if (fst plyr.ai_vars.curpos) mod 2 = 0 then
      if plyr.ai_vars.right > plyr.ai_vars.left then
        if plyr.ai_vars.down > plyr.ai_vars.up then
          if can_move state (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos) + 1) then
              (* +X, +Y *)
              let endpt = (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos) + 1) in
              build_road state (start,endpt)
          else
            if can_move state (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) then
              (* +X *)
              let endpt = (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) in
              build_road state (start,endpt)
            else
              if can_move state (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) then
                (* -X *)
                let endpt = (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) in
                  build_road state (start,endpt)
              else
                let (truefalse, plyr) = curpos_change plyr.roads plyr in
                (* If there are places to put roads, then place it *)
                if truefalse then
                  move_position (change_player state plyr) plyr
                (* If there are no places to put a road, then give the resources
                    back to the player. *)
                else
                  let plyr = change_resource plyr 0 ((get_resource plyr 0)+1) in
                  let plyr = change_resource plyr 4 ((get_resource plyr 4)+1) in
                  change_player state plyr
        else
          if can_move state (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) then
            (* +X *)
            let endpt = (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) in
              build_road state (start,endpt)
          else
            if can_move state (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos) + 1) then
              (* +X, +Y *)
              let endpt = (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos) + 1) in
              build_road state (start,endpt)
            else
              if can_move state (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) then
              (* -X *)
              let endpt = (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) in
                  build_road state (start,endpt)
              else
                let (truefalse, plyr) = curpos_change plyr.roads plyr in
                (* If there are places to put roads, then place it *)
                if truefalse then
                  move_position (change_player state plyr) plyr
                (* If there are no places to put a road, then give the resources
                    back to the player. *)
                else
                  let plyr = change_resource plyr 0 ((get_resource plyr 0)+1) in
                  let plyr = change_resource plyr 4 ((get_resource plyr 4)+1) in
                  change_player state plyr
      else
        if can_move state (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) then
          (* -X *)
          let endpt = (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) in
                  build_road state (start,endpt)
        else
          if can_move state (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) then
            (* +X *)
            let endpt = (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) in
              build_road state (start,endpt)
          else
            if can_move state (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos) + 1) then
              (* +X, +Y *)
              let endpt = (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos) + 1) in
              build_road state (start,endpt)
            else
               let (truefalse, plyr) = curpos_change plyr.roads plyr in
                (* If there are places to put roads, then place it *)
                if truefalse then
                  move_position (change_player state plyr) plyr
                (* If there are no places to put a road, then give the resources
                    back to the player. *)
                else
                  let plyr = change_resource plyr 0 ((get_resource plyr 0)+1) in
                  let plyr = change_resource plyr 4 ((get_resource plyr 4)+1) in
                  change_player state plyr
  else
    if plyr.ai_vars.right < plyr.ai_vars.left then
        if plyr.ai_vars.down > plyr.ai_vars.up then
          if can_move state (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) then
              (* -X *)
                let endpt = (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) in
                  build_road state (start,endpt)
          else
            if can_move state (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) then
              (* +X *)
                let endpt = (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) in
                  build_road state (start,endpt)
            else
              if can_move state (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos) - 1) then
                (* -X, -Y *)
                let endpt = (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos) - 1) in
                  build_road state (start,endpt)
              else
                let (truefalse, plyr) = curpos_change plyr.roads plyr in
                (* If there are places to put roads, then place it *)
                if truefalse then
                  move_position (change_player state plyr) plyr
                (* If there are no places to put a road, then give the resources
                    back to the player. *)
                else
                  let plyr = change_resource plyr 0 ((get_resource plyr 0)+1) in
                  let plyr = change_resource plyr 4 ((get_resource plyr 4)+1) in
                  change_player state plyr
        else
          if can_move state (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos) - 1) then
            (* -X, -Y *)
                let endpt = (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos) - 1) in
                  build_road state (start,endpt)
          else
            if can_move state (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) then
              (* -X *)
                let endpt = (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) in
                  build_road state (start,endpt)
            else
              if can_move state (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) then
              (* +X *)
              let endpt = (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) in
              build_road state (start,endpt)
              else
                let (truefalse, plyr) = curpos_change plyr.roads plyr in
                (* If there are places to put roads, then place it *)
                if truefalse then
                  move_position (change_player state plyr) plyr
                (* If there are no places to put a road, then give the resources
                    back to the player. *)
                else
                  let plyr = change_resource plyr 0 ((get_resource plyr 0)+1) in
                  let plyr = change_resource plyr 4 ((get_resource plyr 4)+1) in
                  change_player state plyr
      else
        if can_move state (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) then
          (* -X *)
          let endpt = (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) in
                  build_road state (start,endpt)
        else
          if can_move state (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) then
            (* +X *)
            let endpt = (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) in
              build_road state (start,endpt)
          else
            if can_move state (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos) - 1) then
            (* -X, -Y *)
                let endpt = (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos) - 1) in
                  build_road state (start,endpt)
          else
               let (truefalse, plyr) = curpos_change plyr.roads plyr in
                (* If there are places to put roads, then place it *)
                if truefalse then
                  move_position (change_player state plyr) plyr
                (* If there are no places to put a road, then give the resources
                    back to the player. *)
                else
                  let plyr = change_resource plyr 0 ((get_resource plyr 0)+1) in
                  let plyr = change_resource plyr 4 ((get_resource plyr 4)+1) in
                  change_player state plyr

let a_i_makemove (state: gamestate): gamestate =
  let player = match_color state.playerturn state.players in
  if (get_resource player 0) > 0 && (get_resource player 4) > 0 then
    let player = change_resource player 0 ((get_resource player 0) - 1) in
    let player = change_resource player 4 ((get_resource player 4) - 1) in
    let gs = move_position state player in
    let plyr = match_color gs.playerturn gs.players in
    if (get_resource player 0) = (get_resource plyr 0) then
      failwith "Build other stuff"
    else
      gs
  else failwith "Build other stuff"