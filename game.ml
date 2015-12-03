open ANSITerminal
open Utilities
open Port
open Board
open Gamestate

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
    (1 + 6 * (x/2) + (x%2) * 2, 6 + 4 * y - 2 * ((x+1)/2))

(* Update a row one pixel at a time. *)
let rec write_str r s str =
  if s<=0 then
    match (r,str) with
    |(hd::tl,a::b)-> a::(write_pixel tl (s-1) b)
    |(_,[]) -> r
    |([],_) -> failwith "Printing outside the board"
  else
    match r with
    |hd::tl -> hd::(write_pixel tl (s-1) str)
    |_ -> failwith "Printing outside the board" in

(* pixboard -> (int * int) -> int -> pixel -> pixboard *)
let write_board pb s str =
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
    let bg = match t.env with
             | Hills -> ('H',[yellow; on_black])
             | Pasture -> ('P',[green; on_black])
             | Mountains -> ('M',[white; on_black])
             | Fields -> ('F',[yellow; on_black])
             | Forest -> ('f',[green; on_black])
             | Desert -> ('d',[yellow; on_black]) in
    (* print tile background *)
    let pb = write_block pb (x+2,y-1) 5 bg in
    let pb = write_block pb (x+1,y) 7 bg in
    let pb = write_block pb (x+2,y-1) 5 bg in
    (* print tile letter *)
    let pb = write_board pb (x+4,y) [(t.loc,[white;on_black])] in
    (* Write the tile number *)
    let num = string_to_pix (string_of_int (t.collect_on)) in
    let pb = write_board pb (x+4,y+1) num in
    if t.robber then write_board pb (x+4,y-1) [('§',[cyan;on_black])] else pb

let print_port pb p =
  let str = match p.exchange with
            |wildcard -> [('R',[white]);('3',[white;on_black])]
            |brick -> [('B',[white]);('2',[white;on_black])]
            |wool -> [('W',[white]);('2',[white;on_black])]
            |grain -> [('G',[white]);('2',[white;on_black])]
            |lumber -> [('L',[white]);('2',[white;on_black])]
            |_ -> failwith "Unmatched port" in
  write_board pb p.location str


(* pixboard -> board -> pixboard *)
let print_board pb b =
  let pb = List.fold_left print_tile pb b.tiles in
  List.fold_left print_port pb b.ports

(* pixboard -> player -> pixboard *)
let print_resources pb p =
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

let print_game gs =
  let pb = board_start in
  let pb = print_board pb gs.game_board in
  let pb = print_resources pb (match_color gs.playerturn gs.players) in
  print_pixboard pb

let test_gs = {Red;[];initialize_board ();Build;false;false}