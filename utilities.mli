(* XY coodinates of the corners of hexagons *)
type coordinates = (int * int)

(* Color of player *)
type color =
  | Red | Blue | White | Orange
(* find the corner coordinates of the tile at a location given by a char*)

type rsrc = int*int*int*int*int

val oob : coordinates list

val corner : char -> (int*int)
(* shuffles a list of objects in a random order *)
val shuffle : 'a list -> 'a list

val string_to_char_list : string -> char list
(* gets the coordinates in the board given a tile location and vertex number*)
val conv : char -> int -> coordinates

val adjacents : coordinates -> coordinates list

val any : ('a -> bool) -> 'a list -> bool

val get_cmd : unit -> string

val is_int : string -> bool

val split_char : char -> string -> string list

val in_bounds : coordinates -> bool