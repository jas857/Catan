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
    [|false;false;false;false;false;true;true; true; true; true; true;|];
    [|false;false;false;false;false;true;true; true; true; true; true;|];
    |]


let initialize_board () =
    {tiles = initialize_tiles ();
     ports = ports;
     dcards = initialize_dcards ();
     blocks = blocks_start
     }

let can_build r c b = b.blocks.(r).(c)
