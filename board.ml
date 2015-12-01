open Tile
open Port
open Dcard

type board = {
    tiles : tile list;
    ports : port list;
    dcards : dcard list
}

let initialize_board () =
    {tiles=initialize_tiles ();
     ports=ports;
     dcards=initialize_dcards ();
     }