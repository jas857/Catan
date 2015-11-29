open Gamestate
(* Dcard module that contains information for development cards *)

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

(* Play a Dcard, do what it says, and print what happens *)
val play_card : gamestate -> dcard -> unit