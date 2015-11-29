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

