open Utilities

let rec find_player (col: color) (lst: player list) : player option =
  match lst with
  | h::t -> if h.color = col then Some h else find_player col t
  | [] -> None

let rec change_player (state: gamestate) (plyr: player) : gamestate =
  let rec change_player_list (lst: player list) (plyr: player) : player list =
    (match lst with
    | h::t -> if h.color = plyr.color then plyr::t else h::(change_player_list t plyr)
    | [] -> [] )  in
  let lst = change_player_list (state.players) (plyr) in
  {state with players = lst}

let get_resource (plyr: player) (resource: int) : int =
  match (resource, plyr.resources) with
  | 0, (x,_,_,_,_) -> x (* Brick *)
  | 1, (_,x,_,_,_) -> x (* Wool *)
  | 2, (_,_,x,_,_) -> x (* Ore *)
  | 3, (_,_,_,x,_) -> x (* Grain *)
  | 4, (_,_,_,_,x) -> x (* Lumber *)
  | _ -> failwith "Something went wrong"

let change_resource (plyr: player) (resource: int) (amt: int) : player =
  match (resource, plyr.resources) with
  | 0, (_,x,y,z,w) -> {plyr with resources = (amt,x,y,z,w)}
  | 1, (x,_,y,z,w) -> {plyr with resources = (x,amt,y,z,w)}
  | 2, (x,y,_,z,w) -> {plyr with resources = (x,y,amt,z,w)}
  | 3, (x,y,z,_,w) -> {plyr with resources = (x,y,z,amt,w)}
  | 4, (x,y,z,w,_) -> {plyr with resources = (x,y,z,w,amt)}

let rec build (state: gamestate) : gamestate =
  let input = read_line() in
  let player = find_player (state.playerturn) (state.players) in
  (match String.lowercase input with
  | "road" -> if (get_resource player 0) > 0 && (get_resource player 4) > 0
          then failwith "TODO"(* BUILD ROAD *)
          else print_endline ("Insufficient resources"); state
  | "settlement" -> if (get_resource player 0) > 0 &&
             (get_resource player 1) > 0 &&
             (get_resource player 3) > 0 &&
             (get_resource player 4) > 0
             then failwith "TODO"(* BUILD SETTLEMENT *)
             else print_endline ("Insufficient resources"); state
  | "city" -> if (get_resource player 3) > 1 &&
           (get_resource player 2) > 2
            then failwith "TODO"(* BUILD CITY *)
            else print_endline ("Insufficient resources"); state
  | "dcard" -> if (get_resource player 1) > 0 &&
           (get_resource player 2) > 0 &&
           (get_resource player 3) > 0
            then failwith "TODO"(* BUILD DCARD *)
            else print_endline ("Insufficient resources"); state
  | _ -> build state)