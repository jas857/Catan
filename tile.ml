open Utilities

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
  |h::tail -> if h.loc = t.loc then t::tail else h::(rebuild_tile_list tail t)

(*clears the robber from the board*)
let rec remove_robber (tl:tile list) =
  match tl with
  |[] -> failwith "no robber on board"
  |h::t -> if h.robber then {h with robber = false} else remove_robber t
