open ANSITerminal
open Utilities
open Port
open Tile
open Board
open Gamestate
open Player

type pixel = char * style list

type pixrow = pixel list

type pixboard = pixrow list

let string_to_pix s : pixrow =
  let cs = string_to_char_list s in
  List.map (fun c -> (c,[white; Bold; on_black])) cs

let board_start =[
"               X - X                                   ";
"              /     \\                                  ";
"         X - X   C   X - X                             ";
"        /     \\     /     \\                            ";
"   X - X   B   X - X   G   X - X                       ";
"  /     \\     /     \\     /     \\                      ";
" 0   A   X - X   F   X - X   L   X                     ";
"  \\     /     \\     /     \\     /        Legend        ";
"   X - X   E   X - X   K   X - X          5 - 4        ";
"  /     \\     /     \\     /     \\        /     \\       ";
" X   D   X - X   J   X - X   P   X      0   L   3      ";
"  \\     /     \\     /     \\     /        \\     /       ";
"   X - X   I   X - X   O   X - X          1 - 2        ";
"  /     \\     /     \\     /     \\              Rates   ";
" X   H   X - X   N   X - X   S   X  Brick:             ";
"  \\     /     \\     /     \\     /   Wool:              ";
"   X - X   M   X - X   R   X - X    Ore:               ";
"        \\     /     \\     /         Grain:             ";
"         X - X   Q   X - X          Lumber:            ";
"              \\     /                                  ";
"               X - X                                   "]


let board_start = List.map string_to_pix board_start

let grid_to_board c =
  let (x,y) = c in
  (1 + 6 * (x/2) + (x mod 2) * 2, 6 + 4 * y - 2 * ((x+1)/2))

(* Update a row one pixel at a time. *)
let rec write_str r s str =
  if s<=0 then
    match (r,str) with
    |(hd::tl,a::b)-> a::(write_str tl (s-1) b)
    |(_,[]) -> r
    |([],_) -> failwith "Printing outside the board"
  else
    match r with
    |hd::tl -> hd::(write_str tl (s-1) str)
    |_ -> failwith "Printing outside the board"

(* pixboard -> (int * int) -> int -> pixel -> pixboard *)
let rec write_board pb s str =
  let (x,y) = s in
  if y>0 then
    match pb with
    |hd::tl -> hd::(write_board tl (x,y-1) str)
    |_ -> failwith "Printing outside the board"
  else
    match pb with
    |row::tl -> (write_str row x str)::tl
    |_ -> failwith "Printing outside the board"

let write_block pb s l p =
  let rec make_block str l p =
    if l<=0 then str else p::(make_block str (l-1) p) in
  write_board pb s (make_block [] l p)


let print_pix (p:pixel) =
  let (c,s) = p in
  printf s "%c" c

let print_row (r:pixrow) =
  let _ = List.map print_pix r in
  print_newline ()

let print_pixboard b =
  List.iter print_row b

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


(* pixboard -> board -> pixboard *)
let print_board pb b =
  let pb = List.fold_left print_tile pb b.tiles in
  pb (* List.fold_left print_port pb b.ports *)

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


(* Print a player's settlements and roads. *)
(* pixboard -> player -> pixboard *)
let print_player pb p =
  failwith "unimplemented"

let rec match_color (c:color) (pl:player list) =
  match pl with
  |[] -> failwith "No player with that color"
  | h::t -> if h.color = c then h else match_color c t

let print_game gs =
  let pb = board_start in
  let pb = print_board pb gs.game_board in
  let pb = print_resources pb (match_color gs.playerturn gs.players) in
  print_pixboard pb

let player1 = {
  roads_left=3;
  roads=[];
  settlements_left=3;
  cities_left=3;
  towns=[];
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