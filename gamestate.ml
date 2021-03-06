open Player
open Board
open Utilities
open Dcard
open Tile
open Port
open Town

(* Gamestate module. Holds information and communicates with all modules. *)

type stage =
  | Start | Production | Build | End

(* State of the game *)
type gamestate = {
  playerturn : color;
  players : player list;
  game_board : board;
  game_stage : stage;
  longest_road_claimed : bool;
  largest_army_claimed : bool
}

(* Takes color [c] and player list [pl] and outputs the player that has
    color [c] *)
let rec match_color (c:color) (pl:player list) =
  match pl with
  |[] -> failwith "No player with that color"
  | h::t -> if h.color = c then h else match_color c t

(* Calls match_color to return the player whose turn it in with the input
    [gs] being the gamestate *)
let curr_player gs = match_color gs.playerturn gs.players

(* Returns the gamestate with the next player's turn if going forward,
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

  (* Returns the gamestate with next player's turn if going backward
      Used only for start stage *)
let next_backward (gs:gamestate) =
  let c = gs.playerturn in
  match c with
  |Red -> {gs with game_stage = Production}
  |Blue -> {gs with playerturn = Red}
  |White -> {gs with playerturn = Blue}
  |Orange -> {gs with playerturn = White}

(* Returns a gamestate with the new players turn during the start stage *)
let choose_next_start (gs:gamestate) =
  let a = curr_player gs in
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


(* Change the stage of the game from Start->Production->Build
    Or changes the turn from one player to another during Build*)
let change_stage (gs:gamestate) =
  match gs.game_stage with
  |Start -> choose_next_start gs
  |Production -> {gs with game_stage = Build}
  |Build -> change_turn gs
  |End -> gs


(* Returns tile given a character location [s] of the tile and list [tiles]*)
let rec get_tile (tiles:tile list) s =
  match tiles with
  |h::t -> if h.loc = (Char.uppercase s) then Some h else get_tile t s
  |_ -> None

(* Adds a new town tuple to a tile town list given gamestate [gs],
    tile to add onto [t] and town to add [ci].
    [ci] denotes the color of the player (color) and the type of town:
      settlement = 1
      town = 2*)
let add_town (gs:gamestate) (t:tile) (ci:color*int) : gamestate =
  let newT = {t with towns = (ci::(t.towns))} in
  let temp_board = gs.game_board in
  let new_board =
  {temp_board with tiles = rebuild_tile_list temp_board.tiles newT} in
  {gs with game_board = new_board}


(* Removes robber from current location, and then places robber in new
location given by the input location character [c]*)
let rec move_robber (gs: gamestate) (c:tile_location): gamestate =
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

(* Takes in player [plyr] and finds the player with [plyr]'s color in [gs], and
    replaces that player with [plyr] *)
let rec change_player (state: gamestate) (plyr: player) : gamestate =
  let lst = change_player_list (state.players) (plyr) in
  {state with players = lst}

(* Checks to see if [changing_player] has the largest army in [state],
    if so, update victory points and largest army fields accordingly *)
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

(* Search through list of towns to find town at [coor] *)
let rec search_towns (coor: coordinates) (towns: town list): bool =
  match towns with
  | [] -> false
  | h::t -> if (coor = h.location)
              then true
            else search_towns coor t

(* Check whether or not it is possible for [p] to build a road at [coor]*)
let is_valid_build_road (coor: coordinates) (p : player): bool =
  let (starts,ends) = List.split p.roads in
  let towns = List.map (fun t -> t.location) p.towns in
  ((List.mem coor starts) || (List.mem coor ends) || (List.mem coor towns)) &&
    in_bounds coor

(* Checks that a road doesn't already exist on the specified edge denoted by
    [road] *)
let rec is_overlap_road (road: coordinates * coordinates)
(players: player list) : bool =
  match players with
  | [] -> false
  | h::t -> let (a,b) = road in
        if ((List.mem road h.roads) || (List.mem (b,a) h.roads))
        then true
        else is_overlap_road road t

(* Gets input for position, given by a tile letter followed by a number. Ex: A5.
    Converts that input into a coordinates type *)
let rec get_pos () =
  let start_tile = read_line () in
  if(String.length start_tile <> 2) then
    let _ = print_string
      "Please enter a tile letter followed by a number 0-5.\n" in get_pos ()
  else
  let start_tile = String.uppercase start_tile in
  let s_tile = start_tile.[0] in
  if not(List.mem s_tile alphabet) then
    let _ = print_string
      "Please enter a tile letter followed by a number 0-5.\n" in
        get_pos ()
  else
  let s_corner = int_of_string (Char.escaped start_tile.[1]) in
  conv s_tile s_corner

(* Gets input of two coordinates (given by get_pos) to build a road on. *)
let rec get_road_info () :(coordinates * coordinates) =
  let _ = print_string
  "Please enter the point you would like to start your road on: " in
  let start = get_pos () in
  let _ = print_string
  "Please enter the point you would like to end your road on: " in
  let last = get_pos () in
  (start, last)

(* Says if road can be built from [fst] to [lst] in gamestate [state]*)
let can_build_road (fst: coordinates)
(lst: coordinates) (state: gamestate) : bool =
  if (not (List.mem fst oob)) && (not (List.mem lst oob)) then
  let currentPlayer = curr_player state in
  (((is_valid_build_road fst currentPlayer)
  ||(is_valid_build_road lst currentPlayer))
  && (not (is_overlap_road (fst, lst) state.players)))
  && List.mem lst (adjacents fst)
  else false

(* Returns player with longest road*)
let rec who_has_longest_road (players: player list) (plyr: player) =
  match players with
  |[] -> plyr
  |h::t -> if(plyr.road_size < h.road_size)
            then h
           else who_has_longest_road t plyr

(* Returns who had the longest road prior to the new road being built *)
let rec old_longest_road_holder (players: player list) (plyr: player) =
  match players with
  |[] -> plyr
  |h::t -> if(h.longest_road)
            then h
           else old_longest_road_holder t plyr

(* Changes gamestate [gs] to update victory
points and booleans based on road being built*)
let rec update_longest_road (gs: gamestate): gamestate =
  let currentP = curr_player gs in
  let newHolder = who_has_longest_road gs.players currentP in
  let oldHolder = old_longest_road_holder gs.players currentP in
  if(newHolder = oldHolder)
    then
      if(newHolder.road_size = 5)
        then change_player {gs with longest_road_claimed = true}
          {newHolder with victory_points = newHolder.victory_points + 2}
        else gs
  else
    let newHolder2 = {newHolder with longest_road = true} in
    let oldHolder2 = {oldHolder with longest_road = false} in
    if(oldHolder2.road_size > 4)
      then let newGs = {gs with longest_road_claimed = true} in
      let newGs2 = change_player newGs
      {newHolder2 with victory_points = newHolder2.victory_points + 2} in
      (change_player newGs2
      {oldHolder2 with victory_points = oldHolder2.victory_points - 2})
    else
      if((newHolder2.road_size = 5) && (oldHolder2.road_size = 4))
        then let newGs = {gs with longest_road_claimed = true} in
        let newGs2 = change_player newGs oldHolder2 in
        (change_player newGs2
        {newHolder2 with victory_points = newHolder2.victory_points + 2})
      else
        let newGs = change_player gs newHolder2 in
        (change_player newGs oldHolder2)


(* Builds the road in game state from fst(coor) to snd(coor)*)
let rec build_road (state: gamestate)
(coor: (coordinates * coordinates)): gamestate =
  let (startTileCoor, endTileCoor) = coor in
  let currentPlayer = curr_player state in
  let updatePlayer = {currentPlayer with
  roads = (((startTileCoor),(endTileCoor))::(currentPlayer.roads));
  roads_left = currentPlayer.roads_left - 1;
  road_size = currentPlayer.road_size + 1} in
  let newGs = change_player state updatePlayer in
  update_longest_road newGs

(* Loop through tiles, if a tile's corners contain the coordinates of the town
    denoted by [coor], then add the town to the towns list of the tile *)
let rec settlement_helper (tiles: tile list)
 (coor: coordinates) (clr: color) : tile list =
  match tiles with
  | [] -> []
  | h::t -> if List.mem coor (corners h)
    then {h with towns = (clr, 1)::(h.towns)}::(settlement_helper t coor clr)
    else h::(settlement_helper t coor clr)

(* Calls get_pos to get coordinates for where to build a settlement *)
let rec get_settlement_info () : coordinates =
  let _ = print_string
    "Please enter the point you would like to build your settlement on: " in
  get_pos ()

(* Checks to see if a settlement can be build at [coor] *)
let can_build_settlement (gs: gamestate) (coor: coordinates): bool =
  if not (in_bounds coor) then false else
  let adjs = adjacents coor in
  (* Checks to see if any players have a town either at that location or at the
      adjacent corners *)
  let blocks_build (pl:player) =
    any (fun t -> List.mem t.location adjs) pl.towns in
  not (any blocks_build gs.players)

(* Returns true if a location [coor] is empty *)
let location_empty (gs: gamestate) (coor: coordinates) : bool =
  (* Checks to see if any players have a town on the location [coor] *)
  let town_on_loc (plyr: player) =
    any (fun t -> t.location = coor) plyr.towns in
  not (any town_on_loc gs.players)

(* Builds a settlement at [coor]. If [free] then do not deduct resources, this
    is only used for the start stage *)
let rec build_settlement (gs: gamestate) (coor: coordinates)
                         (free: bool): gamestate =
  let currentPlayer = curr_player gs in
  let tempPlayer = {currentPlayer with
  settlements_left = currentPlayer.settlements_left - 1;
  towns = {location = coor; pickup = 1}::(currentPlayer.towns);
  victory_points = currentPlayer.victory_points + 1} in
  let gs = change_player gs (if free then tempPlayer else
      change_resources tempPlayer (-1,-1,0,-1,-1)) in
  {gs with game_board = {gs.game_board with
    tiles = settlement_helper gs.game_board.tiles coor currentPlayer.color}}

(* Search for the old settlement at [coor] in the [towns] and replace with city*)
let rec city_helper (towns: town list)
(coor:coordinates) : town list =
  match towns with
  | [] -> []
  | h::t -> if(h.location = coor) then {location = coor; pickup = 2}::t
            else h::(city_helper t coor)

(* Calls get_pos to get coordinates for where to build a city *)
let rec get_city_info (): (coordinates) =
  let _ = print_string
  "Please enter the corner where you would like to build your city: " in
  get_pos ()

(* Checks to see if we can build a city at [coor]*)
let can_build_city (gs: gamestate) (coor: coordinates) : bool =
  if not (in_bounds coor) then false else
  let currentPlayer = curr_player gs in
  any (fun t -> t.pickup=1 && t.location=coor) currentPlayer.towns

(* Builds the city at [coor] by updating the tiles containing [coor]
    and updating the player*)
let rec build_city (gs: gamestate) (coor: coordinates) : gamestate =
  let currentPlayer = curr_player gs in
  let tempPlayer = {currentPlayer with
  towns = (city_helper currentPlayer.towns coor)} in
  let tempPlayer = change_resources tempPlayer (0,0,-3,-2,0) in
  let tempPlayer2 =
    {tempPlayer with victory_points = tempPlayer.victory_points + 1} in
  let gs = change_player gs tempPlayer2 in
  {gs with game_board = {gs.game_board with
    tiles = settlement_helper gs.game_board.tiles coor currentPlayer.color}}

(* Pick a dcard at random in the gamestate [gs] and return a state with the
    current player having that dcard *)
let pick_dcard gs =
  let tempPlayer = curr_player gs in
  let card = List.hd gs.game_board.dcards in
  let _ = print_string ("You picked up a "^(string_of_card card)^" card.\n") in
  let gs = change_player gs
    {tempPlayer with dcards = card::tempPlayer.dcards} in
  let gs ={gs with
    game_board = {gs.game_board with dcards = List.tl gs.game_board.dcards}} in
  match card with
  | Victory_Card _ ->
    change_player gs
      {(curr_player gs) with victory_points =(curr_player gs).victory_points +1}
  | _ -> gs

(* Subtract the cost of a dcard and return the updated gamestate *)
let pick_dcard_subtract_cost (gs:gamestate): gamestate =
  let gs = pick_dcard gs in
  let currentPlayer = curr_player gs in
  let tempPlayer = change_resources currentPlayer (0,-1,-1,-1,0) in
  change_player gs tempPlayer

(* Handles all of the building, checking, and inputting
    [gs] is the gamestate input
    [input] is the string that determines what to build*)
let rec build (gs: gamestate) (input:string): gamestate =
  let player = curr_player gs in
  (match String.lowercase input with
  |"road" -> if (get_resource player 0) > 0 && (get_resource player 4) > 0
                then let gs = build_road gs (get_road_info ()) in
                (*Don't overwrite the new player w/ road in it.*)
                let player = curr_player gs in
                change_player gs (change_resources player (-1,0,0,0,-1))
             else let _ = print_endline ("Insufficient resources\n") in gs
  |"settlement" -> if (get_resource player 0) > 0 &&
             (get_resource player 1) > 0 &&
             (get_resource player 3) > 0 &&
             (get_resource player 4) > 0
             then let sCoor = get_settlement_info () in (* BUILD SETTLEMENT *)
              if(can_build_settlement gs sCoor)
              (*check if settlement can be built*)
                then (build_settlement gs sCoor false)
              else let _ = print_string "cannot build a settlement here\n" in gs
              (*if cannot build a settlement then return original gamestate gs*)
             else let _ = print_endline ("Insufficient resources") in gs

  |"city" -> if (get_resource player 3) > 1 &&
           (get_resource player 2) > 2
            then let cityCoor = get_city_info () in
              if (can_build_city gs cityCoor) (*check if can build city @ coor*)
                  then (build_city gs cityCoor) (*build city if can*)
              else let _ = print_string "cannot build a city here\n" in
              gs (*if cannot build city then return original gamestate gs*)

            else let _ = print_endline ("Insufficient resources") in gs
  |"dcard" -> if (get_resource player 1) > 0 &&
           (get_resource player 2) > 0 &&
           (get_resource player 3) > 0
            then (pick_dcard_subtract_cost gs) (*change the resources*)
            else let _ =print_endline ("Insufficient resources") in gs
  | _ -> gs)

(* Play road building dcard to build two roads [r1] and [r2] *)
let play_road_building (state:gamestate) (r1:(coordinates *coordinates))
(r2:(coordinates * coordinates)): gamestate =
  let state = build_road state r1 in
  build_road state r2

(* Transfer all of the specified resource to the current player,
robbing everyone else.
  [state] is the gamestate
  [r] is the number of the resource to steal (0-4). These numbers are defined
    below in str_to_resource *)
let play_monopoly (state: gamestate) (r: int) : gamestate =
  List.fold_left (fun gs x ->
                  let pl = curr_player gs in
                  let dr = get_resource x r in
                  let pl = change_resource pl r dr in
                  let x = change_resource x r (-dr) in
                  change_player (change_player gs pl) x) state state.players

(* Play year of plenty development card that gives the current player one of
    [resource1] and [resource2] (with these numbers being 0-4) *)
let play_year_plenty
  (state: gamestate) (resource1: int) (resource2: int) : gamestate =
  let plyr = curr_player state in
  let plyr = change_resource plyr resource1 1 in
  let plyr = change_resource plyr resource2 1 in
  change_player state plyr

(* Returns the 0-4 number of the resource that [player] has the least of *)
let min_resource (player: player) : int =
  let resources = [0;1;2;3;4] in
  let rec get_min (lst: int list)=
  (match lst with
  | h::t ->
    let listOfLessThan =
      List.filter (fun x ->
        if get_resource player h <= get_resource player x then true else false)
      resources in
    if (List.length listOfLessThan) == 5 then h else get_min t
  | [] -> 0) in
  get_min resources

(* Converts string [s] to resource number *)
let str_to_resource s =
  match s with
  |"brick" -> 0
  |"wool" -> 1
  |"ore" -> 2
  |"grain" -> 3
  |"lumber" -> 4
  | _ -> failwith "invalid resource name."

(* [gs] is the gamestate
   [sp] is the resource being traded away (0-4)
   [ea] is the resource being traded for (0-4)
   [amt] is the amount of the resource being traded away
  Trades resources for the player with the "bank" *)
let trade gs sp ea amt =
  let pl = curr_player gs in
  if sp=ea then gs else
  let (s,e) = (get_resource pl sp, get_resource pl ea) in
  let exch = get_exchange pl sp in
  let credits = (min s amt)/exch in
  change_player gs
    (change_resource (change_resource pl sp (-exch*credits)) ea credits)

(* [gs] is the gamestate
   [spend] is the string denoting the resource being traded away
   [earn] is the string denoting the resource being traded for
   [amt] is the amount of [spend] being traded away *)
let prep_trade gs spend earn amt =
  let sp = str_to_resource spend in
  let ea = str_to_resource earn in
  trade gs sp ea amt

(* Add [amt] of the specified resource, denoted by [env] to [player]. *)
let change_resource_for_distr (plyr: player) (env:environment)
(amt: int) : player =
  match (env, plyr.resources) with
  | Hills, (a,x,y,z,w) -> {plyr with resources = (amt + a,x,y,z,w)}
  | Pasture, (x,a,y,z,w) -> {plyr with resources = (x,amt + a,y,z,w)}
  | Mountains, (x,y,a,z,w) -> {plyr with resources = (x,y,amt + a,z,w)}
  | Fields, (x,y,z,a,w) -> {plyr with resources = (x,y,z,amt + a,w)}
  | Forest, (x,y,z,w,a) -> {plyr with resources = (x,y,z,w,amt + a)}
  | Desert, (a,x,y,z,w) -> plyr

(* Distribute the resources for a tile based on its towns field.
Return a new player list with updated resource amounts.*)
let rec dist_resources (players:player list)
 (towns:(color * int) list) (env:environment): player list =
  match towns with
  | [] -> players
  |(b, a)::t -> let tempPlayer = match_color b players in
                let newPlayers = change_player_list players
                (change_resource_for_distr tempPlayer env a) in
                dist_resources newPlayers t env

(* Collect resources from all tiles for all players, return a new
player list with updated resource amounts. *)
let rec collect_player_resource (players: player list)
(tiles: tile list) (roll: int): player list =
  let ts = (List.filter (fun t -> t.collect_on=roll) tiles) in
  List.fold_left
    (fun (pl:player list) (t:tile)-> dist_resources pl t.towns t.env) players ts

(* AI Functions *)

(* AI builds settlement for [player] at [loc].
    If [free], then do not deduct resources *)
let ai_build_settlement
  (state:gamestate) (player: player) (loc:coordinates) (free:bool) =
  let gs = build_settlement state loc true in
  let player = curr_player gs in
  let _ = print_endline ("AI built settlement") in
  let _ = player.ai_vars.curpos <- loc in
  {gs with players = change_player_list gs.players player}

(* AI builds road for [player] from [start] to [endpt] *)
let ai_build_road
  (state:gamestate) (player:player) (start:coordinates) (endpt:coordinates) =
  let gs = (build_road state (start,endpt)) in
  let _ = print_endline ("AI built road") in
  let player = curr_player gs in
  let _ = player.ai_vars.curpos <- endpt in
  {gs with players = change_player_list gs.players player}

(*Loop through [tiles] and record where they are in relation to current position
    Set the player.ai_varr.up/down/left/right to the number of "good" tiles,
     with the collect_on being betwen 4 and 10, to that direction of curpos.
    curpos is the "position" of the player, which gets updated to the most
      recent settlement of road built by the player*)
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

(* Calls loop_tiles to update left/right/up/down directions for [plyr] *)
let ai_update_directions (state: gamestate) (plyr: player) : unit =
  (plyr.ai_vars.left <- 0);
  (plyr.ai_vars.right <- 0);
  (plyr.ai_vars.up <- 0);
  (plyr.ai_vars.down <- 0);
  loop_tiles state.game_board.tiles plyr

(* Checks if the [coord] has a road coming from curpos into it.
  Also checks if there is a town at [coord].
  Also checks if it is off the board *)
let can_move (state: gamestate) (coord: coordinates): bool =
  (can_build_road (curr_player state).ai_vars.curpos coord state)
  && location_empty state coord && not(List.mem coord oob)

(* AI moves position based on up/down/left/right of [plyr] and builds a road
    there. If no roads can be build immediately next to curpos in the best
    direction (the one with the highest number of good tiles), then change
    curpos to a position on [plyr]'s chain of roads. Do this up to six times
    with [depth] indicating which time it is on. *)
let rec move_position (state: gamestate) (plyr: player) (depth:int): gamestate =
  if depth > 5 then
    let plyr = change_resources plyr (1,0,0,0,1) in
                  change_player state plyr
  else
  let start = plyr.ai_vars.curpos in
  let _ = ai_update_directions state plyr in
  if (fst plyr.ai_vars.curpos) mod 2 = 0 then
      if plyr.ai_vars.right > plyr.ai_vars.left then
        if plyr.ai_vars.down > plyr.ai_vars.up then
          if can_move state
            (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos) + 1) then
              (* +X, +Y *)
              let endpt =
              (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos) + 1) in
              ai_build_road state plyr start endpt
          else
            if can_move state
              (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) then
              (* +X *)
              let endpt =
                (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) in
              ai_build_road state plyr start endpt
            else
              if can_move state
                (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) then
                (* -X *)
                let endpt =
                  (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) in
                ai_build_road state plyr start endpt
              else
                let (truefalse, plyr) =
                  curpos_change plyr.roads plyr state.players in
                (* If there are places to put roads, then place it *)
                if truefalse then
                  move_position (change_player state plyr) plyr (depth + 1)
                (* If there are no places to put a road, then give the resources
                    back to the player. *)
                else
                  let plyr = change_resources plyr (1,0,0,0,1) in
                  change_player state plyr
        else
          if can_move state
            (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) then
            (* +X *)
            let endpt =
              (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) in
            ai_build_road state plyr start endpt
          else
            if can_move state
             (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos) + 1) then
              (* +X, +Y *)
              let endpt =
               (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos) + 1) in
              ai_build_road state plyr start endpt
            else
              if can_move state
                (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) then
              (* -X *)
              let endpt =
                (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) in
              ai_build_road state plyr start endpt
              else
                let (truefalse, plyr) =
                  curpos_change plyr.roads plyr state.players in
                (* If there are places to put roads, then place it *)
                if truefalse then
                  move_position (change_player state plyr) plyr (depth + 1)
                (* If there are no places to put a road, then give the resources
                    back to the player. *)
                else
                  let plyr = change_resources plyr (1,0,0,0,1) in
                  change_player state plyr
      else
        if can_move state
          (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) then
          (* -X *)
          let endpt =
            (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) in
          ai_build_road state plyr start endpt
        else
          if can_move state
            (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) then
            (* +X *)
            let endpt =
              (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) in
            ai_build_road state plyr start endpt
          else
            if can_move state
              (fst (plyr.ai_vars.curpos) +1, snd (plyr.ai_vars.curpos) + 1) then
              (* +X, +Y *)
              let endpt =
               (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos) + 1) in
              ai_build_road state plyr start endpt
            else
               let (truefalse, plyr) =
                  curpos_change plyr.roads plyr state.players in
                (* If there are places to put roads, then place it *)
                if truefalse then
                  move_position (change_player state plyr) plyr (depth + 1)
                (* If there are no places to put a road, then give the resources
                    back to the player. *)
                else
                  let plyr = change_resources plyr (1,0,0,0,1) in
                  change_player state plyr
  else
    if plyr.ai_vars.right < plyr.ai_vars.left then
        if plyr.ai_vars.down > plyr.ai_vars.up then
          if can_move state
            (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) then
              (* -X *)
                let endpt =
                  (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) in
                ai_build_road state plyr start endpt
          else
            if can_move state
              (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) then
              (* +X *)
                let endpt =
                  (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) in
                ai_build_road state plyr start endpt
            else
              if can_move state
              (fst (plyr.ai_vars.curpos) -1, snd (plyr.ai_vars.curpos) - 1) then
                (* -X, -Y *)
                let endpt =
                 (fst (plyr.ai_vars.curpos) -1, snd (plyr.ai_vars.curpos) -1) in
                ai_build_road state plyr start endpt
              else
                let (truefalse, plyr) =
                  curpos_change plyr.roads plyr state.players in
                (* If there are places to put roads, then place it *)
                if truefalse then
                  move_position (change_player state plyr) plyr (depth + 1)
                (* If there are no places to put a road, then give the resources
                    back to the player. *)
                else
                  let plyr = change_resources plyr (1,0,0,0,1) in
                  change_player state plyr
        else
          if can_move state
            (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos) - 1) then
            (* -X, -Y *)
                let endpt =
                 (fst (plyr.ai_vars.curpos) -1, snd (plyr.ai_vars.curpos) -1) in
                ai_build_road state plyr start endpt
          else
            if can_move state
              (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) then
              (* -X *)
                let endpt =
                  (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) in
                ai_build_road state plyr start endpt
            else
              if can_move state
                (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) then
              (* +X *)
              let endpt =
                (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) in
              ai_build_road state plyr start endpt
              else
                let (truefalse, plyr) =
                  curpos_change plyr.roads plyr state.players in
                (* If there are places to put roads, then place it *)
                if truefalse then
                  move_position (change_player state plyr) plyr (depth + 1)
                (* If there are no places to put a road, then give the resources
                    back to the player. *)
                else
                  let plyr = change_resources plyr (1,0,0,0,1) in
                  change_player state plyr
      else
        if can_move state
            (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) then
          (* -X *)
          let endpt =
            (fst (plyr.ai_vars.curpos) - 1, snd (plyr.ai_vars.curpos)) in
          ai_build_road state plyr start endpt
        else
          if can_move state
            (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) then
            (* +X *)
            let endpt =
              (fst (plyr.ai_vars.curpos) + 1, snd (plyr.ai_vars.curpos)) in
            ai_build_road state plyr start endpt
          else
            if can_move state
              (fst (plyr.ai_vars.curpos) -1, snd (plyr.ai_vars.curpos) - 1) then
            (* -X, -Y *)
                let endpt =
                (fst (plyr.ai_vars.curpos) -1, snd (plyr.ai_vars.curpos) -1) in
                ai_build_road state plyr start endpt
          else
               let (truefalse, plyr) =
                curpos_change plyr.roads plyr state.players in
                (* If there are places to put roads, then place it *)
                if truefalse then
                  move_position (change_player state plyr) plyr (depth + 1)
                (* If there are no places to put a road, then give the resources
                    back to the player. *)
                else
                  let plyr = change_resources plyr (1,0,0,0,1) in
                  change_player state plyr

(* Play development card [card] and call specific functions for which dcard *)
let play_dcard (state: gamestate) (card: dcard) : gamestate =
  let player = curr_player state in
  if List.mem card player.dcards then
  let player = {player with dcards = (remove_from_list player.dcards card)} in
  (match card with
  | Knight -> if player.a_i then
      let tiles = state.game_board.tiles in
      let randTile = List.nth tiles (Random.int (List.length tiles)) in
      move_robber
      (check_largest_army state {player with army_size = player.army_size + 1})
      randTile.loc
  else let _ = print_string
  "Please enter the letter of the tile you would like to move the robber to: \n"
  in
  let input = read_line() in
  if (Bytes.length input) > 1
  then (let _ = print_endline "Invalid tile location" in state)
  else move_robber
      (check_largest_army state {player with army_size = player.army_size + 1})
      (Bytes.get input 0)
  | Victory_Card (name, desc) ->
       let _ = (print_endline ("You played: " ^ name ^ "- " ^ desc)) in
       let _ =print_endline ("Points for Victory Cards are awarded on pickup")in
       state
  | Progress_Card p -> (match p with
               | Monopoly ->
                if player.a_i then
                  play_monopoly state (min_resource player)
                else
                let num = get_input true
                ("Input one type of resource to take from all opponents\n
                (0 = Brick, 1 = Wool, 2 = Ore, 3 = Grain, 4 = Lumber):") in
                play_monopoly state num
               | Year_of_plenty ->
                if player.a_i then
                  play_year_plenty state (min_resource player)
                                   ((min_resource player + 1) mod 4)
                else
                let num = get_input false
                ("Input two types of resources to take with no space\n
                (0 = Brick, 1 = Wool, 2 = Ore, 3 = Grain, 4 = Lumber):") in
                if num < 10 then play_year_plenty state 0 num
                else
                  play_year_plenty state ((num -(num mod 10))/10) (num mod 10)
               | Road_Building ->
                  if player.a_i then
                   let gs = move_position state (curr_player state) 0 in
                    move_position gs (curr_player gs) 0
                  else
                  let r1 = get_road_info () in
                  let r2 = get_road_info () in
                  play_road_building state r1 r2))
  else let _ = print_endline "You do not have that dcard" in state

(* AI [player] checks to see if it has enough resources to build a dcard,
    if not, return the input gamestate [state] *)
let ai_check_build_dcard (state: gamestate) (player: player) : gamestate =
  if (get_resource player 1) > 0 && (get_resource player 2) > 0 &&
      (get_resource player 3) > 0 then
      pick_dcard state
  else
    state

(* AI [player] checks to see if it has enough resources to build a road,
    if so, it calls move_position and checks if a road was built.
    If either of these checks come up negative, then check if a dcard can be
    built *)
let ai_check_build_road (state: gamestate) (player: player): gamestate =
  if (get_resource player 0) > 0 && (get_resource player 4) > 0 then
    (* Build road *)
    let _ = print_endline("roadbuilding") in
    let player = change_resources player (-1,0,0,0,-1) in
    let gs = move_position state player 0 in
    let plyr = curr_player gs in
    (* If no road was built *)
    if (get_resource player 0) = (get_resource plyr 0) then
      ai_check_build_dcard state player
    else
      gs
  else
    ai_check_build_dcard state player

(* AI [player] checks to see if it has enough resources to build a settlement,
    if so, it tries to build one and checks if a settlement was built.
    If either of these checks come up negative, then check if a road can be
    built *)
let ai_check_build_settlement (state: gamestate) (player: player) : gamestate =
  let _ = print_endline(string_of_int (get_resource player 0)) in
  let _ = print_endline(string_of_int (get_resource player 1)) in
  let _ = print_endline(string_of_int (get_resource player 3)) in
  let _ = print_endline(string_of_int (get_resource player 4)) in
  if (get_resource player 0) > 0 && (get_resource player 1) > 0 &&
      (get_resource player 3) > 0 && (get_resource player 4) > 0 then
      (* Build Settlement *)
      let rec match_roads (roads: (coordinates * coordinates) list) =
      (match roads with
      | h::t -> if can_build_settlement state (fst h) then
                  let _ = print_endline ("settlements1") in
                    ai_build_settlement state player (fst h) false
                else if can_build_settlement state (snd h) then
                let _ = print_endline ("settlements2") in
                    ai_build_settlement state player (snd h) false
                else match_roads t
      | [] -> let _ = print_endline ("settlements4") in state) in
      let _ = print_endline ("settlements3") in
      match_roads player.roads
  else
  let _ = print_endline ("settlementsNONE") in
    ai_check_build_road state player

(* Looks through the player's towns to see if any can be upgraded,
    starting with the ones on tiles more likely to be rolled *)
let rec ai_look_for_upgrades (state:gamestate) (num1:int) (num2:int):gamestate =
  if num1 > 12 || num2 < 2 then state
  else
  let tiles = List.filter
    (fun tile -> tile.collect_on = num1 || tile.collect_on = num2)
    state.game_board.tiles in
  let corners = List.fold_left (fun acc t -> acc@(corners t)) [] tiles in
  let player = curr_player state in
  let settlements = List.filter (fun twn -> twn.pickup = 1) player.towns in
  let avail = List.filter (fun s -> List.mem s.location corners) settlements in
  if (List.length avail) = 0 then
    ai_look_for_upgrades (state) (num1+1) (num2-1)
  else
    let toUpgrade = List.nth avail (Random.int (List.length avail)) in
    build_city state (toUpgrade.location)

(* AI [player] checks to see if it has enough resources to build a city,
    if so, it calls ai_look_for_upgrades and checks if a city was built.
    If either of these checks come up negative then check if a settlement can be
    built *)
let ai_check_build_city (state: gamestate) (player: player) : gamestate =
  if (get_resource player 2) > 2 && (get_resource player 3) > 1 then
      (* Build city *)
      ai_look_for_upgrades state 8 6
  else
    ai_check_build_settlement (state) (player)

(* AI makes a building move in the order specified below *)
let a_i_makemove (state: gamestate): gamestate =
  (* AI builds things in this order:
      1. City
      2. Settlement
      3. Road
      4. Dcard *)
  let player = curr_player state in
  ai_check_build_city state player

(* Returns all playable Dcards of [player] *)
let has_dcards_to_play (player: player) : dcard list =
  List.fold_left (fun lst d ->
    match d with Knight -> d::lst | Progress_Card _ -> d::lst | _ -> lst)
  []
  player.dcards

(* Pick a random playable dcard from [dcards] and play it *)
let ai_play_dcard (state: gamestate) (dcards: dcard list) : gamestate =
let temp = List.nth dcards (Random.int (List.length dcards)) in
if(temp = (Progress_Card(Monopoly))) then let _ = 
print_string "AI played Monopoly \n" in play_dcard state (temp)
else play_dcard state (temp)

(* AI conducts trading. For the first resource that is higher than its exchange
    rate, trade it for the resource with the least amount *)
let ai_trade (state: gamestate): gamestate =
  let player = curr_player state in
  let (a,b,c,d,e) = player.resources in
    if a >= (get_exchange player 0) then
      trade state 0 (min_resource player) (get_exchange player 0)
    else
    if b >= (get_exchange player 1) then
      trade state 1 (min_resource player) (get_exchange player 1)
    else
    if c >= (get_exchange player 2) then
      trade state 2 (min_resource player) (get_exchange player 2)
    else
    if d >= (get_exchange player 3) then
      trade state 3 (min_resource player) (get_exchange player 3)
    else
    if e >= (get_exchange player 4) then
      trade state 4 (min_resource player) (get_exchange player 4)
    else
    state

(* AI plays dcads if it has them, if not, it calls a_i_makemove *)
let ai_build_or_play (state: gamestate): gamestate =
  let state = ai_trade state in
  let player = curr_player state in
  let dcards_to_play = has_dcards_to_play player in
  if (List.length dcards_to_play) > 0 then
    (* Play Dcard *)
    change_stage (ai_play_dcard state dcards_to_play)
  else
    (* Build *)
    change_stage (a_i_makemove state)

(* AI plays dcards if it has them, if not, it rolls the dice *)
let ai_roll_or_play (state: gamestate): gamestate =
  let player = curr_player state in
  let dcards_to_play = has_dcards_to_play player in
  if (List.length dcards_to_play) > 0 then
    (* Play Dcard *)
    change_stage (ai_play_dcard state dcards_to_play)
  else
    (* Roll *)
    let _ = Random.self_init () in
             let rnd = (((Random.int 6) + 2) + Random.int 6) in
             let playersWResources =
             collect_player_resource state.players state.game_board.tiles rnd in
             if rnd = 7 then
              let tiles = state.game_board.tiles in
              let randTile = List.nth tiles (Random.int (List.length tiles)) in
              move_robber (check_largest_army state
                          {player with army_size = player.army_size + 1})
              randTile.loc
             else
             change_stage {state with players = playersWResources}

(* Builds a settlement for the AI on tiles with num1 or num2 as the collect_on.
    If there are no tiles or no buildable corners on the returned tiles, search
    with a higher number. If there are no tiles at all, just return the state *)
let rec build_settlement_on_num
  (state: gamestate) (num1: int) (num2: int): gamestate =
  let rec match_tiles (tile_lst: tile list) (gs: gamestate): gamestate =
  (match tile_lst with
  | h::t -> if can_build_settlement gs (conv h.loc 0) then
              ai_build_settlement gs (curr_player gs) (conv h.loc 0) (true)
            else if can_build_settlement gs (conv h.loc 1) then
              ai_build_settlement gs (curr_player gs) (conv h.loc 1) (true)
            else if can_build_settlement gs (conv h.loc 2) then
              ai_build_settlement gs (curr_player gs) (conv h.loc 2) (true)
            else if can_build_settlement gs (conv h.loc 3) then
              ai_build_settlement gs (curr_player gs) (conv h.loc 3) (true)
            else if can_build_settlement gs (conv h.loc 4) then
              ai_build_settlement gs (curr_player gs) (conv h.loc 4) (true)
            else if can_build_settlement gs (conv h.loc 5) then
              ai_build_settlement gs (curr_player gs) (conv h.loc 5) (true)
          else match_tiles t gs
  | [] -> gs) in
  if num1 > 12 || num2 < 2 then state
  else
  let tiles = List.filter
    (fun tile -> (tile.env = Hills || tile.env = Forest)
              && (tile.collect_on = num1 || tile.collect_on = num2))
    state.game_board.tiles in
  let gs = match_tiles tiles state in
  if gs = state then build_settlement_on_num gs (num1+1) (num2-1) else gs

(* Builds a settlement for the start stage using the helper function above *)
let ai_start_stage_settlement (state: gamestate): gamestate =
  build_settlement_on_num state 8 6

(* Builds a road for the start stage coming from the settlement made *)
let ai_start_stage_road (state: gamestate): gamestate =
  let plyr = curr_player state in
  let start = plyr.ai_vars.curpos in
  if can_build_road (start) (fst (start) + 1, snd (start)) state then
      (* +X *)
        let endpt = (fst (start) + 1, snd (start)) in
        let gs = change_stage (build_road state (start,endpt)) in
        let player = curr_player gs in
        let _ = player.ai_vars.curpos <- endpt in
        {gs with players = change_player_list gs.players player}
      else
        if can_build_road (start) (fst (start) - 1, snd (start)) state then
        (* -X *)
          let endpt = (fst (start) - 1, snd (start)) in
          let gs = change_stage (build_road state (start,endpt)) in
          let player = curr_player gs in
          let _ = player.ai_vars.curpos <- endpt in
          {gs with players = change_player_list gs.players player}
        else
          if (fst (start) mod 2 = 0) then
            if can_build_road (start) (fst (start) + 1, snd (start) + 1) state
              then
              (* +X, +Y *)
              let endpt = (fst (start) + 1, snd (start) + 1) in
              let gs = change_stage (build_road state (start,endpt)) in
              let player = curr_player gs in
              let _ = player.ai_vars.curpos <- endpt in
              {gs with players = change_player_list gs.players player}
            else failwith "Never happens in start stage"
          else
            if can_build_road (start) (fst (start) - 1, snd (start) - 1) state
              then
              (* -X, -Y *)
              let endpt = (fst (start) - 1, snd (start) - 1) in
              let gs = change_stage (build_road state (start,endpt)) in
              let player = curr_player gs in
              let _ = player.ai_vars.curpos <- endpt in
              {gs with players = change_player_list gs.players player}
            else failwith "Never happens in start stage"

(* Builds a settlement and road for the AI in the start stage *)
let ai_start_stage (state: gamestate): gamestate =
  let gs = ai_start_stage_settlement state in
  ai_start_stage_road gs
