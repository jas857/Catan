open Tile
open Port
open Dcard

type board = {
    tiles : tile list;
    ports : port list;
    dcards : dcard list;
    blocks : bool array array
}

let blocks_start =
    [|
    [| true; true; true; true; true;true;true;false;false;false;false;|];
    [| true; true; true; true; true;true;true; true; true;false;false;|];
    [| true; true; true; true; true;true;true; true; true; true;false;|];
    [|false; true; true; true; true;true;true; true; true; true; true;|];
    [|false;false;false; true; true;true;true; true; true; true; true;|];
<<<<<<< HEAD
    [|false;false;false;false;false;true;true; true; true; true; true;|]
=======
    [|false;false;false;false;false;true;true; true; true; true; true;|];
>>>>>>> bf640b7f8970aeb2854405dcc474ed85478cb292
    |]


let initialize_board () =
    {tiles = initialize_tiles ();
     ports = ports;
     dcards = initialize_dcards ();
     blocks = blocks_start
     }