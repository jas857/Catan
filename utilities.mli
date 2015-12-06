(* XY coodinates of the corners of hexagons *)
type coordinates = (int * int)

(* Color of player *)
type color =
  | Red | Blue | White | Orange

(* Convenience type for the 5-tuple used to represent resources *)
type rsrc = int*int*int*int*int

(* List of letters with matching tiles. *)
val alphabet : char list

(* List of coordinates which are inside the 5x11 bounds but off the
actual board. *)
val oob : coordinates list

(* Returns the leftmost corner coordinate of the specified tile. *)
val corner : char -> (int*int)
(* Randomize the order of list d. *)
val shuffle : 'a list -> 'a list
(* Break a string down into a char list *)
val string_to_char_list : string -> char list

(* Convert a letter, number position to coordinates. *)
val conv : char -> int -> coordinates

(* Return a list of coordinates one edge away from the input, plus the
input itself for collision checking functions. *)
val adjacents : coordinates -> coordinates list

(* Return true if the pred is true for any member of l. *)
val any : ('a -> bool) -> 'a list -> bool

(* Read a lowercase command from stdin. *)
val get_cmd : unit -> string

(* Check if a string is an int. *)
val is_int : string -> bool

(* From RosettaCode, splits a string with a character as a delimiter.*)
val split_char : char -> string -> string list

(* Check whether coordinate c is within the bounds of the board. *)
val in_bounds : coordinates -> bool