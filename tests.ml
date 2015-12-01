open Tile
open Player
open Board
open Gamestate
open Assertions
open Utilities


(*Test Tile *)
let ranTiles = initialize_tiles ()

let rec check_for_robber (tl:tile list) (b:bool) =
  List.fold_left (fun a x -> (a || x.robber)) false tl

TEST_UNIT = List.length ranTiles === 19
TEST_UNIT = check_for_robber ranTiles false === true

let tempDesert = remove_robber ranTiles

TEST_UNIT =  tempDesert.env === Desert

let newTiles = rebuild_tile_list ranTiles tempDesert

TEST_UNIT = check_for_robber newTiles false === false



(*Test Dcards *)



(*Test Gamestate *)
let ranBoard = initialize_board ()

let buildstate = {playerturn = Red; players = initialize_non_ai_players ();
game_board = ranBoard; game_stage = Build; longest_road_claimed = false;
largest_army_claimed = false}

let prodstate = {playerturn = Red; players = initialize_non_ai_players ();
game_board = ranBoard; game_stage = Production; longest_road_claimed = false;
largest_army_claimed = false}

let tradestate = {playerturn = Red; players = initialize_non_ai_players ();
game_board = ranBoard; game_stage = Trade; longest_road_claimed = false;
largest_army_claimed = false}


(*Test change turn, and change_stage during playing stages*)
(*End of Red's turn *)
let next = change_turn buildstate
TEST_UNIT = next === {prodstate with playerturn = Blue}
(*End of Blue's Production stage *)
let progress = change_stage next
TEST_UNIT = progress === {tradestate with playerturn = Blue}
(*End of Blue's trade stage *)
let progress2 = change_stage progress
TEST_UNIT = progress2 === {buildstate with playerturn = Blue}
(*End of Blue's turn *)
let next = change_turn progress2
TEST_UNIT = next === { prodstate with playerturn = White}
(*End of White's Production stage *)
let progress = change_stage next
TEST_UNIT = progress === {tradestate with playerturn = White}
(*End of White's Trade stage *)
let progress2 = change_stage progress
TEST_UNIT = progress2 === {buildstate with playerturn = White}
(*End of White's Turn *)
let next = change_turn progress2
TEST_UNIT = next === {prodstate with playerturn = Orange}
(*End of Orange's Production stage *)
let progress = change_stage next
TEST_UNIT = progress === {tradestate with playerturn = Orange}
(*End of Orange's Trade stage *)
let progress2 = change_stage progress
TEST_UNIT = progress2 === {buildstate with playerturn = Orange}
(*End of Orange's Turn *)
let next = change_turn progress2
TEST_UNIT = next === {prodstate with playerturn = Red}


(*Testing for change_player *)
let redPlayer = init_non_ai_player Red
let bluePlayer = init_non_ai_player Blue
let whitePlayer = init_non_ai_player White
let orangePlayer = init_non_ai_player Orange

let addRedVP = {redPlayer with victory_points = 1}
let newPList = [addRedVP;bluePlayer;whitePlayer;orangePlayer]

TEST_UNIT = change_player buildstate addRedVP ===
            {buildstate with players = newPList}
(* add some more cases here *)


(*Testing for change_stage during start stage,
need tests for change_player as well *)

let startstate = {playerturn = Red; players = initialize_non_ai_players ();
game_board = ranBoard; game_stage = Start; longest_road_claimed = false;
largest_army_claimed = false}


let next = change_stage (change_player startstate
{redPlayer with settlements_left = 4; roads_left = 14})

TEST_UNIT = next.playerturn === Blue

let next2 = change_stage (change_player next
{bluePlayer with settlements_left = 4; roads_left = 14})
TEST_UNIT = next2.playerturn === White

let next = change_stage (change_player next2
{whitePlayer with settlements_left = 4; roads_left = 14})
TEST_UNIT = next.playerturn === Orange

let next2 = change_stage (change_player next
{orangePlayer with settlements_left = 4; roads_left = 14})
TEST_UNIT = next2.playerturn === Orange

let next = change_stage (change_player next2
{orangePlayer with settlements_left = 3; roads_left = 13})
TEST_UNIT = next.playerturn === White

let next2 = change_stage (change_player next
{whitePlayer with settlements_left = 3; roads_left = 13})
TEST_UNIT = next2.playerturn === Blue

let next = change_stage (change_player next2
{bluePlayer with settlements_left = 3; roads_left = 13})
TEST_UNIT = next.playerturn === Red

let newStage = change_stage (change_player next
{redPlayer with settlements_left = 3; roads_left = 13})
TEST_UNIT = newStage.game_stage === Production











