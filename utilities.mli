(* XY coodinates of the corners of hexagons *)
type coordinates = (int * int)

(* Color of player *)
type color =
  | Red | Blue | White | Orange
(* find the corner coordinates of the tile at a location given by a char*)
val corner : char -> (int*int)
(* shuffles a list of objects in a random order *)
val shuffle : 'a list -> 'a list

val string_to_char_list : string -> char list