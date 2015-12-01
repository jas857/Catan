open Tile
open Player
open Board
open Gamestate
open Assertions


(*Test Tile *)
let ranBoard = initialize_tiles ()

let rec check_for_robber (tl:tile list) (b:bool) =
  List.fold_left (fun a x -> (a || x.robber)) false tl

TEST_UNIT = List.length ranBoard === 19
TEST_UNIT = check_for_robber ranBoard false === true

let tempDesert = remove_robber ranBoard

TEST_UNIT =  tempDesert.env === Desert

let newBoard = rebuild_tile_list ranBoard tempDesert

TEST_UNIT = check_for_robber newBoard false === false










