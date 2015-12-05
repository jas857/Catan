(* Name of development card *)
type name = string

(* Description of development card *)
type description = string

type pcard =
  | Monopoly
  | Year_of_plenty
  | Road_Building

type dcard =
  | Knight
  | Progress_Card of pcard
  | Victory_Card of (name * description)

let rec remove_from_list (dcards: dcard list) (card: dcard) : dcard list =
  match dcards with
  | h::t -> if h = card then t else h::(remove_from_list t card)
  | [] -> []

let rec get_input (monopoly: bool) (message: string) : int =
  print_string message;
  let input = read_line() in
    (match int_of_string input with
     | s -> if monopoly then
          if s >= 0 && s <= 4 then s else get_input monopoly message
        else
          if s >= 0 && s <= 44 then
             if s mod 10 <= 4 then s else get_input monopoly message
            else get_input monopoly message)

let rec initialize_dcards_helper ans =
    let _ = Random.self_init () in
    let randomList = List.map (fun x -> ((Random.int 2000), x)) ans in
    let sortedList = (List.sort compare randomList) in
    List.map (fun (x,y) -> y) sortedList

let initialize_dcards (): dcard list =
    let deckAns = [Knight; Victory_Card(("University", "A place of learning"));
    Knight; Victory_Card(("Chapel", "A place of worship"));
    Knight; Victory_Card(("Market", "A place of trading"));
    Knight; Victory_Card(("Palace", "A place of ruling"));
    Knight; Knight; Progress_Card(Year_of_plenty);
    Knight; Progress_Card(Road_Building);
    Knight; Progress_Card(Year_of_plenty); Knight;
    Progress_Card(Road_Building); Knight;
    Victory_Card(("Library", "A place of studying")); Knight;
    Knight; Progress_Card(Monopoly); Progress_Card(Monopoly);
    Knight; Knight] in
    initialize_dcards_helper deckAns
