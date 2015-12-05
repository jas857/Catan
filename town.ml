open Utilities

type town = {
	location : coordinates;
	pickup : int
}

let twn c p = {location=c;pickup=p}
