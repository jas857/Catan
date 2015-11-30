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


(*Initial list of numbers to collect on *)
let numbers = [5;2;6;3;8;10;9;12;11;4;8;10;9;4;5;6;3;11]
(*Initial list of environments *)
let envs = [Hills;Hills;Hills;
            Pasture;Pasture;Pasture;Pasture;
            Mountains;Mountains;Mountains;
            Fields;Fields;Fields;Fields;
            Forest;Forest;Forest;Forest;
            Desert]


let rec make_tiles (tl:tile list) (nl:int list) (el:environment list)
 (c:tile_location)=

  let newLoc = char_of_int ((int_of_char c) +1) in
  let newC = corner newLoc in
  match (nl,el) with
  |(_,Desert::a) -> make_tiles ({env = Desert;
                  collect_on = 0;
                  loc = newLoc;
                  corner = newC;
                  towns = [];
                  robber = false;
                  }::tl) nl a newLoc
  |(n1::n2,e1::e2) ->  make_tiles ({env = e1;
                  collect_on = n1;
                  loc = newLoc;
                  corner = newC;
                  towns = [];
                  robber = false;
                  }::tl) n2 e2 newLoc
  |_ -> []


let initialize_tiles () =
  let shuffNum = shuffle numbers in
  let shuffEnv = shuffle envs in
  make_tiles [] shuffNum shuffEnv '@'
