open Tile
open Port
open Dcard

type board = {
    tiles : tile list;
    ports : port list;
    dcards : dcard list
}

let initialize_board () =
    {initialize_tiles ();
     ports;
     initialize_dcards ();
     }