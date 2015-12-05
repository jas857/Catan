open ANSITerminal
open Utilities
open Port
open Tile
open Board
open Gamestate
open Player
open Town
open Dcard
open Initial

type pixel = char * style list

type pixrow = pixel list

type pixboard = pixrow list

(* Behaves like normal print_string. *)
let print_string_w s = print_string [white;on_black] s

let string_to_pix_st s st =
  let cs = string_to_char_list s in
  List.map (fun c -> (c,st)) cs

let string_to_pix s =
  string_to_pix_st s [white; Bold; on_black]

let board_start =[
"               X---X    Current turn:                            ";
"              /     \\                                            ";
"         X---X   C   X---X          Hills:     X       Legend    ";
"        /     \\     /     \\         Pasture:   X        5---4    ";
"   X---X   B   X---X   G   X---X    Mountains: X       /     \\   ";
"  /     \\     /     \\     /     \\   Fields:    X      0   L   3  ";
" X   A   X---X   F   X---X   L   X  Forest:    X       \\     /   ";
"  \\     /     \\     /     \\     /   Desert:    X        1---2    ";
"   X---X   E   X---X   K   X---X                                 ";
"  /     \\     /     \\     /     \\                                ";
" X   D   X---X   J   X---X   P   X                               ";
"  \\     /     \\     /     \\     /                                ";
"   X---X   I   X---X   O   X---X                                 ";
"  /     \\     /     \\     /     \\                                ";
" X   H   X---X   N   X---X   S   X             Rates             ";
"  \\     /     \\     /     \\     /   Brick:                       ";
"   X---X   M   X---X   R   X---X    Wool:                        ";
"        \\     /     \\     /         Ore:                         ";
"         X---X   Q   X---X          Grain:                       ";
"              \\     /               Lumber:                      ";
"               X---X                                             "]


let board_start = List.map string_to_pix board_start

(* Convert hex grid coordinates to screenspace coordinates *)
let grid_to_board c =
  let (x,y) = c in
  (1 + 6 * (x/2) + (x mod 2) * 2, 6 + 4 * y - 2 * ((x+1)/2))

(* Update a row so that the given pixel list overwrites pixels starting
at the given index.*)
(* Update a row one pixel at a time. *)
let rec write_row r s str =
  if s<=0 then
    match (r,str) with
    |(hd::tl,a::b)-> a::(write_row tl (s-1) b)
    |(_,[]) -> r
    |([],_) -> failwith "Printing outside the board"
  else
    match r with
    |hd::tl -> hd::(write_row tl (s-1) str)
    |_ -> failwith "Printing outside the board"

(* Create a copy of the board where the input list is
written horizontally starting at x,y.*)
(* pixboard -> (int * int) -> int -> pixel -> pixboard *)
let rec write_board pb s str =
  let (x,y) = s in
  if y>0 then
    match pb with
    |hd::tl -> hd::(write_board tl (x,y-1) str)
    |_ -> failwith "Printing outside the board"
  else
    match pb with
    |row::tl -> (write_row row x str)::tl
    |_ -> failwith "Printing outside the board"

(* Create a copy of the board where the input pixel overwrites pixels
from x,y to x+l,y *)
let write_block pb s l p =
  let rec make_block str l p =
    if l<=0 then str else p::(make_block str (l-1) p) in
  write_board pb s (make_block [] l p)

(* Print a single character with ANSITerminal formatting *)
let print_pix (p:pixel) =
  let (c,s) = p in
  printf s "%c" c

(* Print a list of pixels as if it were a string. *)
let print_row (r:pixrow) =
  let _ = List.map print_pix r in
  print_newline ()

(* Print a list of lists of pixels. *)
let print_pixboard b =
  List.iter print_row b

(* Create a copy of the board with the tile printed over it. *)
let print_tile pb t =
  let (x,y) = grid_to_board t.corner in
  let bg = (match t.env with
           | Hills -> ('H',[yellow; on_black])
           | Pasture -> ('P',[green; on_black])
           | Mountains -> ('M',[white; on_black])
           | Fields -> ('F',[yellow; on_black])
           | Forest -> ('f',[green; on_black])
           | Desert -> ('d',[yellow; on_black])) in
  (* print tile background *)
  let pb = write_block pb (x+2,y-1) 5 bg in
  let pb = write_block pb (x+1,y) 7 bg in
  let pb = write_block pb (x+2,y+1) 5 bg in
  (* print tile letter *)
  let pb = write_board pb (x+4,y) [(t.loc,[white;on_black;Bold])] in
  (* Write the tile number *)
  let num = string_to_pix (string_of_int (t.collect_on)) in
  let pb = write_board pb (x+4,y+1) num in
  let robber = [('R',[cyan; on_black])] in
  if t.robber then
    write_board pb ((x+4),(y-1)) robber
   else pb
(* Print a port onto the pixboard as a 2 character symbol
marking the type of trade available and the rate. *)
let print_port pb (p:port) =
  let str = match p.exchange with
            |(3, 3, 3, 3, 3) -> [('R',[Bold]);('3',[white;Bold;on_black])]
            |(2, 4, 4, 4, 4) -> [('B',[Bold]);('2',[white;Bold;on_black])]
            |(4, 2, 4, 4, 4) -> [('W',[Bold]);('2',[white;Bold;on_black])]
            |(4, 4, 2, 4, 4) -> [('O',[Bold]);('2',[white;Bold;on_black])]
            |(4, 4, 4, 2, 4) -> [('G',[Bold]);('2',[white;Bold;on_black])]
            |(4, 4, 4, 4, 2) -> [('L',[Bold]);('2',[white;Bold;on_black])]
            |_ -> failwith "Unmatched port" in
  write_board pb (grid_to_board p.location) str


let print_biomes pb =
  let legend = [
   ([('H',[yellow; on_black])],2);
   ([('P',[green; on_black])],3);
   ([('M',[white; on_black])],4);
   ([('F',[yellow; on_black])],5);
   ([('f',[green; on_black])],6);
   ([('d',[yellow; on_black])],7)] in
  List.fold_left (fun p (s,y) -> write_board p (47,y) s) pb legend

(* Initialize a new printed board with tile info and grid.*)
(* pixboard -> board -> pixboard *)
let print_board b =
  let pb = board_start in
  let pb = List.fold_left print_tile pb b.tiles in
  let pb = print_biomes pb in
  List.fold_left print_port pb b.ports

(* Create a copy of the board with the player's resources and trade rates
printed in the correct place. *)
(* pixboard -> player -> pixboard *)
let print_resources pb (p:player) =
  (* resources start at 15 44 *)
  let (b,w,o,g,l)=p.resources in
  let (be,we,oe,ge,le)=p.exchange in
  let resources = [(15,b,be);(16,w,we);(17,o,oe);(18,g,ge);(19,l,le)] in
  let print_one pb (y,r,e) =
    let gap = if r<10 then "   " else "  " in
    let s = string_to_pix ((string_of_int r) ^ gap ^ (string_of_int e)) in
    write_board pb (44,y) s in
  List.fold_left print_one pb resources


(* Create a copy of the board with the player's roads and settlements printed
over it. *)
(* pixboard -> player -> pixboard *)
let print_player pb p =
  let st = match p.color with
  | Red -> [red; Bold; on_black]
  | Blue -> [blue; Bold; on_black]
  | White -> [cyan; Bold; on_black]
  | Orange -> [green; Bold; on_black] in
  let print_town pb (t:town) =
    let c = if t.pickup=1 then 'S' else 'C' in
    write_board pb (grid_to_board t.location) [(c,st)] in
  let pb = List.fold_left print_town pb p.towns in
  let print_road pb (fst,lst) =
    let ((x,y),(a,b))=(fst,lst) in
    let diff = (a-x,b-y) in
    let (dx,dy,str) = match (x mod 2, diff) with
                |(0,(-1,0)) -> (-3,0,"---")
                |(0,(1,0)) -> (1,-1,"/")
                |(0,(1,1)) -> (1,1,"\\")
                |(1,(-1,0)) -> (-1,1,"/")
                |(1,(1,0)) -> (1,0,"---")
                |(1,(-1,-1)) -> (-1,-1,"\\")
                |(_,(_,_)) -> failwith "Malformed road" in
    let (r,c) = grid_to_board fst in
    let (r,c) = (r+dx,c+dy) in
    write_board pb (r,c) (string_to_pix_st str st) in
  List.fold_left print_road pb p.roads

let print_turn pb gs =
  let text, col = match gs.playerturn with
             |Red -> ("Red", red)
             |Blue -> ("Blue", blue)
             |White -> ("Cyan", cyan)
             |Orange -> ("Green", green) in
  write_board pb (38,0) (string_to_pix_st text [col;Bold;on_black])


let print_game gs =
  let pb = print_board gs.game_board in
  let pb = print_resources pb (match_color gs.playerturn gs.players) in
  let pb = List.fold_left print_player pb gs.players in
  let pb = print_turn pb gs in
  print_pixboard pb

(* Add amt of the specified resource to player. *)
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
  List.fold_left (fun (pl:player list) (t:tile) -> dist_resources
    pl t.towns t.env) players ts

let rec match_to_Dcard () =
  let _ = print_string_w "Which dcard will you play? " in
  let cmd = get_cmd () in
  match cmd with
  |"knight" -> Some(Knight)
  |"monopoly" -> Some(Progress_Card(Monopoly))
  |"year of plenty" -> Some(Progress_Card(Year_of_plenty))
  |"road" -> Some(Progress_Card(Road_Building))
  |"exit" -> None
  | _ -> let _ = print_string_w "That doesn't appear to be a dcard type. Please choose from [knight, monopoly, year of plenty, road] or exit to play nothing.\n"
  in match_to_Dcard ()

let min5 j k =
  let (a,b,c,d,e) = j in
  let (v,w,x,y,z) = k in
  (min a v, min b w, min c x, min d y, min e z)

(* Updates the current player's exchange rates from their settlements and
  cities. *)
let update_exchanges gs =
  let pl = curr_player gs in
  let check_port (b:player) (a:port) =
    if any (fun t -> t.location = a.location) b.towns then
      {b with exchange= min5 a.exchange b.exchange}
    else b in
  change_player gs (List.fold_left check_port {pl with exchange=(4,4,4,4,4)}
    gs.game_board.ports)

(* Perform the start stage of the game. When complete, every player
should have two settlements and two roads. *)
let rec start_repl gs =
if (curr_player gs).a_i then
  ai_start_stage gs
else
  let gs = update_exchanges gs in
  let rec start_settlement gs : gamestate=
    let _ = print_game gs in
    let coor = get_settlement_info () in
    if not (can_build_settlement gs coor) then
      let _ = print_string_w "Cannot build a settlement there.\n" in
      start_settlement gs
    else build_settlement gs coor true in
  let gs = start_settlement gs in
  let rec start_road gs : gamestate=
    let _ = print_game gs in
    let (s, e) = get_road_info () in
    if can_build_road s e gs then change_stage (build_road gs (s,e)) else
      let _ = print_string_w "Cannot build a road there, please try again.\n" in
      start_road gs in
  start_road gs

(* Perform the trade phase of the game. Trade command format:
"trade X brick wool" will spend at most X bricks to purchase wool. *)
let rec trade_repl gs : gamestate =
  let _ = print_game gs in
  let _ = print_string_w "Please enter a command in the following format, or \"end\" to end trading:
    \"trade [int] [resource to spend] [resource to purchase]\"\n" in
  let cmd = split_char ' ' (get_cmd ()) in
  if List.nth cmd 0 = "end" then gs else
  if (List.length cmd) <> 4 then
    trade_repl gs
  else
    trade_repl (trade gs
      (List.nth cmd 2) (List.nth cmd 3) (int_of_string (List.nth cmd 1)))

let rec prod_repl (gs : gamestate) : gamestate =
if (curr_player gs).a_i then
  ai_roll_or_play gs
else
  let gs = update_exchanges gs in
  let _ = print_game gs in
  let _ = print_string_w
    "type roll or play to roll the die or play a development card.\n" in
  let cmd = (get_cmd ()) in
  match cmd with
  |"roll" -> let _ = Random.self_init () in
             let rnd = (((Random.int 6) + 2) + Random.int 6) in
             let playersWResources =
             collect_player_resource gs.players gs.game_board.tiles rnd in
             let gs = {gs with players = playersWResources} in
             let _ = printf [white;Bold] "You rolled a %d\n" rnd in
             change_stage gs

  |"play" -> (match (match_to_Dcard ()) with
                | Some(x) -> (let ans = play_dcard gs x in
                              if(ans = gs)
                                then prod_repl gs
                              else ans)
                | None -> gs)
  | _ -> gs


let rec build_repl (gs : gamestate) : gamestate =
if (curr_player gs).a_i then
  ai_build_or_play gs
else
  let _ = print_game gs in
  let _ = print_string_w
   "type buy, play, trade, or end to build something, play a development card, begin trading or end your turn\n" in
  let lowercaseCmd = String.lowercase (get_cmd ()) in
  match lowercaseCmd with
  |"buy" -> let _ = print_string_w "What would you like to buy? [road settlement city dcard]?\n" in
              build gs (get_cmd ())

  |"play" -> (match (match_to_Dcard ()) with
                | Some(x) -> let ans = play_dcard gs x in
                              if(ans = gs)
                                then prod_repl gs
                              else ans
                | None -> gs)
  |"trade" -> build_repl (trade_repl gs)
  |"end" -> change_stage gs

  | _ -> gs

let rec main_repl (gs: gamestate) : gamestate =
  match gs.game_stage with
  | Start -> main_repl (start_repl gs)
  | Production -> main_repl (prod_repl gs)
  | Build -> main_repl (build_repl gs)
  | End -> gs

(* let _ = main_repl trade_gs *)
let _ = main_repl default_gs
