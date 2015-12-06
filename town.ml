open Utilities

(* Town module that contains the location of a town and whether it's
a settlement or city as an int that also serves as its pickup multiplier.*)
type town = {
	location : coordinates;
	pickup : int
}

(* Convenience constructor for towns, only used in initial.ml for
hardcoded test states. *)
let twn c p = {location=c;pickup=p}
