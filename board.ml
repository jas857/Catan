open Tile
open Port
open Dcard

type board = {
    tiles : tile list;
    ports : port list;
    dcards : dcard list;
    blocks : bool array array
}

let oob = [(3,0); (4,0); (4,1); (4,2); (5,0); (5,1); (5,2); (5,3); (5,4); (0,8);
           (0,9); (0,10); (0,11); (1,10); (1,11); (2,11)]

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
