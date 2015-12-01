open ANSITerminal
open Utilities

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
" X   H   X - X   N   X - X   S   X  Brick:  XX  XX     ";
"  \\     /     \\     /     \\     /   Wool:   XX  XX     ";
"   X - X   M   X - X   R   X - X    Ore:    XX  XX     ";
"        \\     /     \\     /         Grain:  XX  XX     ";
"         X - X   Q   X - X          Lumber: XX  XX     ";
"              \\     /                                  ";
"               X - X                                   "]


let board_start = List.map string_to_pix board_start

let print_pix (p:pixel) =
    let (c,s) = p in
    printf s "%c" c

let print_row (r:pixrow) =
    let _ = List.map print_pix r in
    print_newline ()

let print_pixboard gs =
    List.iter print_row gs

let _ = print_pixboard board_start

