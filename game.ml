open ANSITerminal
open Utilities
open Port
open Tile
open Board
open Gamestate
open Player
open Town
open Dcard

type pixel = char * style list

type pixrow = pixel list

type pixboard = pixrow list

let string_to_pix_st s st =
  let cs = string_to_char_list s in
  List.map (fun c -> (c,st)) cs

let string_to_pix s =
  string_to_pix_st s [white; Bold; on_black]

let board_start =[
"               X---X                                   ";
"              /     \\                                  ";
"         X---X   C   X---X                             ";
"        /     \\     /     \\                            ";
"   X---X   B   X---X   G   X---X                       ";
"  /     \\     /     \\     /     \\                      ";
" 0   A   X---X   F   X---X   L   X                     ";
"  \\     /     \\     /     \\     /        Legend        ";
"   X---X   E   X---X   K   X---X          5---4        ";
"  /     \\     /     \\     /     \\        /     \\       ";
" X   D   X---X   J   X---X   P   X      0   L   3      ";
"  \\     /     \\     /     \\     /        \\     /       ";
"   X---X   I   X---X   O   X---X          1---2        ";
"  /     \\     /     \\     /     \\              Rates   ";
" X   H   X---X   N   X---X   S   X  Brick:             ";
"  \\     /     \\     /     \\     /   Wool:              ";
"   X---X   M   X---X   R   X---X    Ore:               ";
"        \\     /     \\     /         Grain:             ";
"         X---X   Q   X---X          Lumber:            ";
"              \\     /                                  ";
"               X---X                                   "]


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
            |(3, 3, 3, 3, 3) -> [('R',[white]);('3',[white;on_black])]
            |(2, 4, 4, 4, 4) -> [('B',[white]);('2',[white;on_black])]
            |(4, 2, 4, 4, 4) -> [('W',[white]);('2',[white;on_black])]
            |(4, 4, 2, 4, 4) -> [('O',[white]);('2',[white;on_black])]
            |(4, 4, 4, 2, 4) -> [('G',[white]);('2',[white;on_black])]
            |(4, 4, 4, 4, 2) -> [('L',[white]);('2',[white;on_black])]
            |_ -> failwith "Unmatched port" in
  write_board pb p.location str

(* Initialize a new printed board with tile info and grid.*)
(* pixboard -> board -> pixboard *)
let print_board pb b =
  let pb = List.fold_left print_tile pb b.tiles in
  pb (* List.fold_left print_port pb b.ports *)

(* Create a copy of the board with the player's resources and trade rates
printed in the correct place. *)
(* pixboard -> player -> pixboard *)
let print_resources pb (p:player) =
  (* resources start at 14 44 *)
  let (b,w,o,g,l)=p.resources in
  let (be,we,oe,ge,le)=p.exchange in
  let resources = [(14,b,be);(15,w,we);(16,o,oe);(17,g,ge);(18,l,le)] in
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

let print_game gs =
  let pb = board_start in
  let pb = print_board pb gs.game_board in
  let pb = print_resources pb (match_color gs.playerturn gs.players) in
  let pb = List.fold_left print_player pb gs.players in
  print_pixboard pb
(* Test content for printing *)
let player1 = {
  roads_left=3;
  roads=[((2,2),(3,3));((3,3),(4,3));((4,3),(5,3))];
  settlements_left=3;
  cities_left=3;
  towns=[{location=(2,2);pickup=1};{location=(7,3);pickup=2}];
  victory_points=0;
  dcards=[];
  resources = (2,3,4,5,6);
  exchange = (4,4,4,4,4);
  color = Red;
  a_i = false;
  ai_vars = {curpos= (0,0); left=0;right=0;up=0;down=0};
  army_size=0;
  largest_army=false;
  road_size=0;
  longest_road = false
}

let test_gs = {playerturn=Red;
               players=[player1];
               game_board=initialize_board ();
               game_stage=Build;
               longest_road_claimed=false;
               largest_army_claimed=false}

let _ = print_game test_gs
let _ = print_string [white;Bold] "This is a text prompt asking for some input.\n"
(* End print testing *)

(* Behaves like normal print_string. *)
let print_string_w s = print_string [white;Bold;on_black]

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
  List.fold_left (fun (pl:player list) (t:tile) -> dist_resources pl t.towns t.env) players ts

let rec match_to_Dcard () =
  let _ = print_string_w "Which dcard will you play?" in
  let cmd = get_cmd () in
  match cmd with
  |"knight" -> Some(Knight)
  |"monopoly" -> Some(Progress_Card(Monopoly))
  |"year of plenty" -> Some(Progress_Card(Year_of_plenty))
  |"road building" -> Some(Progress_Card(Road_Building))
  |"exit" -> None
  | _ -> let _ = print_string_w "cannot play this at the moment"
  in match_to_Dcard ()




let rec rollOrPlay (cmd: string) (gs : gamestate) : gamestate =
  let lowercaseCmd = String.lowercase cmd in
  match lowercaseCmd with
  |"roll" -> let _ = Random.self_init () in
             let rnd = (((Random.int 6) + 2) + Random.int 6) in
             let playersWResources =
             collect_player_resource gs.players gs.game_board.tiles rnd in
             change_stage {gs with players = playersWResources}

  |"play" -> (match (match_to_Dcard ()) with
                | Some(x) -> (let ans = play_dcard gs x in
                              if(ans = gs)
                                then rollOrPlay "play" gs
                              else ans)
                | None -> gs)
  | _ -> gs


let rec buildOrPlay (cmd: string) (gs : gamestate) : gamestate =
  let lowercaseCmd = String.lowercase cmd in
  match lowercaseCmd with
  |"build" -> let _ = print_string_w "What would you like to build?" in
              let cmd2 = read_line() in
              build gs cmd2

  |"play" -> (match (match_to_Dcard ()) with
                | Some(x) -> let ans = play_dcard gs x in
                              if(ans = gs)
                                then rollOrPlay "play" gs
                              else ans
                | None -> gs)
  |"end" -> change_stage gs

  | _ -> gs




let rec main_repl (gs: gamestate) : gamestate =
  match gs.game_stage with
  | Start -> let _ = print_game gs (*prints game*) in
             let coor = get_settlement_info () in (*get settlement coordinates*)
              (*checks if can build settlement UNIMPLENTED*)
             if( not (can_build_settlement gs coor))
               then let _ = print_string_w "invalid inputs, redo turn" in
                main_repl gs (*wrong input*)
             else let gs1 = build_settlement gs coor in(*correct input*)
             let (s, e) = get_road_info() in (*get road coordinates*)
             if( not (can_build_road s e gs)) then
             let _ = print_string_w "invalid inputs, redo turn" in main_repl gs
             else main_repl (change_stage (build_road gs1 (s,e)))
             (*build road then change turn*)
  | Production -> let _ = print_string_w
            "type roll or play: roll the die or play a development card" in
            let cmd = get_cmd () in
             main_repl (rollOrPlay cmd gs)
  | Trade -> failwith "TODO"(*TO BE IMPLEMENTED*)
  | Build -> let _ = print_string_w
            "type build, play, or end: build something,
             play a development card or end your turn" in
            let cmd = get_cmd () in
             main_repl (buildOrPlay cmd gs)
  | End -> gs