open Gamestate

type environment =
  | Hills | Pasture | Mountains | Fields | Forest | Desert

type tile_location = char

type tile = {
  env : environment;
  collect_on : int;
  loc : tile_location;
  corner : coordinates;
  towns : (color * int) list;
  robber : bool
}

let rec rebuild_tile_list (tl:tile list) (t:tile) =
  match tl with
  |[] -> []
  |h::tail -> if h.loc = t.loc then t::tail else h::(find_tile tail t)

let add_town (gs:gamestate) (t:tile) (ci:color*int) =
  let newT = {t with towns = ci::(t.towns)} in
  let temp_board = gs.game_board in
  let new_board = {temp_board with tiles = rebuild_tile_list tiles t} in
  {gs with game_board = new_board}