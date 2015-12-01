open Utilities

type port = {
	location : coordinates;
    (* Br Wo Or Gr Lu *)
	exchange : (int * int * int * int * int)
}

let ports =[

{location=(0, 0);exchange=(3, 3, 3, 3, 3)};
{location=(1, 0);exchange=(3, 3, 3, 3, 3)};

{location=(3, 0);exchange=(4, 2, 4, 4, 4)};
{location=(4, 0);exchange=(4, 2, 4, 4, 4)};

{location=(7, 1);exchange=(3, 3, 3, 3, 3)};
{location=(8, 1);exchange=(3, 3, 3, 3, 3)};

{location=(10, 2);exchange=(3, 3, 3, 3, 3)};
{location=(11, 3);exchange=(3, 3, 3, 3, 3)};

{location=(10, 4);exchange=(2, 4, 4, 4, 4)};
{location=(11, 4);exchange=(2, 4, 4, 4, 4)};

{location=(8, 5);exchange=(4, 4, 4, 4, 2)};
{location=(9, 5);exchange=(4, 4, 4, 4, 2)};

{location=(5, 5);exchange=(3, 3, 3, 3, 3)};
{location=(6, 5);exchange=(3, 3, 3, 3, 3)};

{location=(2, 3);exchange=(4, 4, 4, 2, 4)};
{location=(3, 4);exchange=(4, 4, 4, 2, 4)};

{location=(0, 1);exchange=(4, 4, 2, 4, 4)};
{location=(1, 2);exchange=(4, 4, 2, 4, 4)};
]