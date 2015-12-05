open Utilities

type port = {
	location : coordinates;
    (* Br Wo Or Gr Lu *)
	exchange : (int * int * int * int * int)
}

let wildcard = (3, 3, 3, 3, 3)
let brick = (2, 4, 4, 4, 4)
let wool = (4, 2, 4, 4, 4)
let ore = (4, 4, 2, 4, 4)
let grain = (4, 4, 4, 2, 4)
let lumber = (4, 4, 4, 4, 2)

let ports =[

{location=(0, 0);exchange=wildcard};
{location=(1, 0);exchange=wildcard};

{location=(3, 0);exchange=wool};
{location=(4, 0);exchange=wool};

{location=(7, 1);exchange=wildcard};
{location=(8, 1);exchange=wildcard};

{location=(10, 2);exchange=wildcard};
{location=(11, 3);exchange=wildcard};

{location=(10, 4);exchange=brick};
{location=(11, 4);exchange=brick};

{location=(8, 5);exchange=lumber};
{location=(9, 5);exchange=lumber};

{location=(5, 5);exchange=wildcard};
{location=(6, 5);exchange=wildcard};

{location=(2, 3);exchange=grain};
{location=(3, 4);exchange=grain};

{location=(0, 1);exchange=ore};
{location=(1, 2);exchange=ore};
]
