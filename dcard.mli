open Utilities
(* Dcard module that contains information for development cards *)

(* Name of victory card *)
type name = string

(* Description of victory card *)
type description = string

(* Progress Card variant that contains the three types of progress cards
    defined in the Catan rules *)
type pcard =
  | Monopoly
  | Year_of_plenty
  | Road_Building

(* Development Card variant that contains the three types of development cards
    defined in the Catan rules *)
type dcard =
  | Knight
  | Progress_Card of pcard
  | Victory_Card of (name * description)

(* Play a Dcard, do what it says, and print what happens *)
val remove_from_list : dcard list -> dcard -> dcard list

(* Gets an input for playing a monopoly or year of plenty card and parses it. *)
val get_input : bool -> string -> int

(* Generates the stack fo dcards in a random order *)
val initialize_dcards : unit -> dcard list

(* Converts a DCard to a string, so the user can know what card he picks up
    or plays *)
val string_of_card : dcard -> string