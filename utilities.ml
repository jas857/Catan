
type coordinates = (int * int)

(* Convenience type for the 5-tuple used to represent resources *)
type rsrc = int*int*int*int*int

(* List of letters with matching tiles. *)
let alphabet = ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';
                'P';'Q';'R';'S']

(* Randomize the order of list d. *)
let shuffle d =
  let _ = Random.self_init() in
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

(* Returns the leftmost corner coordinate of the specified tile. *)
let corner t =
    match t with
    |'A' -> (0, 0)
    |'B' -> (2, 0)
    |'C' -> (4, 0)
    |'D' -> (0, 1)
    |'E' -> (2, 1)
    |'F' -> (4, 1)
    |'G' -> (6, 1)
    |'H' -> (0, 2)
    |'I' -> (2, 2)
    |'J' -> (4, 2)
    |'K' -> (6, 2)
    |'L' -> (8, 2)
    |'M' -> (2, 3)
    |'N' -> (4, 3)
    |'O' -> (6, 3)
    |'P' -> (8, 3)
    |'Q' -> (4, 4)
    |'R' -> (6, 4)
    |'S' -> (8, 4)
    | _ -> failwith "OutOfBoundsException"

(* List of coordinates which are inside the 5x11 bounds but off the
actual board. *)
let oob = [(0,3); (0,4); (1,4); (2,4); (0,5); (1,5); (2,5); (3,5); (4,5); (7,0);
            (8,0); (9,0); (9,1); (10,0); (11,0); (10,1); (11,1); (11,2); (12,4);
            (12,3)]

(* Convert a letter, number position to coordinates. *)
let conv t n =
    let (r,c) = corner t in
    let (x,y) = match n with
              |0 -> (0,0)
              |1 -> (1,1)
              |2 -> (2,1)
              |3 -> (3,1)
              |4 -> (2,0)
              |5 -> (1,0)
              |_ -> failwith "OutOfBoundsException" in
    (r+x,c+y)

(* Break a string down into a char list *)
let string_to_char_list s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* Player colors, named for the original game's colors. For printing
we had to use different colors because our library didn't have orange. *)
type color =
  | Red | Blue | White | Orange

(* Return a list of coordinates one edge away from the input, plus the
input itself for collision checking functions. *)
let adjacents (x,y) =
  let adjs = if (x mod 2) = 0 then
    [(x-1,y);(x+1,y);(x+1,y+1);(x,y)]
  else
    [(x-1,y);(x+1,y);(x-1,y-1);(x,y)] in
  let check (x,y) = (x>=0) && (y>=0) && not (List.mem (x,y) oob) in
  List.filter check adjs

(* Return true if the pred is true for any member of l. *)
let rec any f l =
  match l with
      |a::b -> if f a then true else any f b
      |[] -> false

(* Read a lowercase command from stdin. *)
let get_cmd () = String.lowercase (read_line ())

(* Check if a string is an int. *)
let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

(* From RosettaCode, splits a string with a character as a delimiter.*)
let rec split_char sep str =
  try
    let i = String.index str sep in
    String.sub str 0 i ::
      split_char sep (String.sub str (i+1) (String.length str - i - 1))
  with Not_found ->
    [str]

(* Check whether coordinate c is within the bounds of the board. *)
let in_bounds c =
  let (x,y) = c in
  (x>=0) && (y>=0) && (x<12) && (y<6) && not (List.mem c oob)