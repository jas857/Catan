open Gamestate

(* Town module that contains the location of a town and whether it
	is a settlement or city [the pickup multiplier] *)

type town = {
	location : coordinates;
	pickup : int
}