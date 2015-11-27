let play_monopoly (state: gamestate) (resource: int) : gamestate =
  let toAdd = List.fold_left (fun acc x ->
                  if x.color = state.playerturn
                  then acc
                  else acc + (Player.get_resource x resource))
                 0 state.players in
  let plyrs = List.map (fun x ->
              if x.color = state.playerturn
              then Player.change_resource x resource
                (toAdd +(Player.get_resource x resource))
              else Player.change_resource x resource 0) in
  state with players = plyrs

let play_year_plenty
  (state: gamestate) (resource1: int) (resource2: int) : gamestate =
  let plyr = Player.find_player (state.playerturn) (state.players) in
  let plyr = Player.change_resource plyr resource1
        ((Player.get_resource plyr resource1) + 1) in
  let plyr = Player.change_resource plyr resource2
        ((Player.get_resource plyr resource2) + 1) in
  Players.change_player state plyr

let play_road_building = failwith "TODO" (* Call Road Building Method(s) *)

let rec remove_from_list (dcards: dcard list) (card: dcard) : dcard list =
  match dcards with
  | h::t -> if h = card then t else h::(remove_from_list t card)
  | [] -> []

let rec get_input (monopoly: bool) (message: String) : int =
  print_string message;
  let input = read_line() in
    (match int_of_string input with
     | Failure _ -> get_input monopoly message
     | s -> if monopoly then
          if s >= 0 && s <= 4 then s else get_input monopoly message
        else
          if s >= 0 && s <= 44 then
             if s mod 10 <= 4 then s else get_input monopoly message
            else get_input monopoly message)

let play_card (state: gamestate) (card: dcard) : gamestate =
  let player = Player.find_player (state.playerturn) (state.players) in
  if List.mem card player.dcards then
  let player = player with dcards = (remove_from_list player.dcards card) in
  (match card with
  | Knight -> Board.move_robber
  | Victory_Card (name, desc) ->
       (print_endline ("You played: " ^ name ^ "- " ^ desc));
       let plyr = player with victory_points = player.victory_points + 1 in
       if plyr.victory_points = 10 then state = state with stage = End else
       Player.change_player (state) (plyr)
  | Progress_Card p -> (match p with
               | Monopoly ->
          let num = get_input true
          ("Input one type of resource to take from all opponents\n
          (0 = Brick, 1 = Wool, 2 = Ore, 3 = Grain, 4 = Lumber):") in
          play_monopoly state num
               | Year_of_plenty ->
          let num = get_input false
          ("Input two types of resources to take with no space\n
          (0 = Brick, 1 = Wool, 2 = Ore, 3 = Grain, 4 = Lumber):") in
          if num < 10 then play_year_plenty 0 num
          else play_year_plenty ((num -(num mod 10))/10) (num mod 10)
               | Road_Building -> (* Do road building action *)))
  else state