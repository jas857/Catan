open Player
open Board
open Utilities

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
let game_compelte (gs:gamestate) =
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
  let new_board = {temp_board with tiles = rebuild_tile_list tiles t} in
  {gs with game_board = new_board}