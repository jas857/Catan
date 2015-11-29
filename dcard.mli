open Gamestate
(* Dcard module that contains information for development cards *)

(* Name of development card *)
type name = string

(* Description of development card *)
type description = string

type dcard =
	| Knight of (name * description)
	| Progress_Card of (name * description)
	| Victory_Card of (name * description)

(* Play a Dcard, do what it says, and print what happens *)
val play_card : dcard -> unit