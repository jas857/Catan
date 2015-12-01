
type coordinates = (int * int)

let shuffle d =
  let _ = Random.self_init() in
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let corner t =
    match t with
    |'A' -> (0, 0)
    |'B' -> (2, 0)
    |'C' -> (4, 0)
    |'D' -> (0, 1)
    |'E' -> (2, 1)
    |'F' -> (4, 1)
    |'G' -> (6, 1)
    |'H' -> (0, 2)
    |'I' -> (2, 2)
    |'J' -> (4, 2)
    |'K' -> (6, 2)
    |'L' -> (8, 2)
    |'M' -> (2, 3)
    |'N' -> (4, 3)
    |'O' -> (6, 3)
    |'P' -> (8, 3)
    |'Q' -> (4, 4)
    |'R' -> (6, 4)
    |'S' -> (8, 4)
    | _ -> failwith "OutOfBoundsException"

let conv t n =
    let (r,c) = corner t in
    let (x,y) = match n with
              |0 -> (0,0)
              |1 -> (1,1)
              |2 -> (2,1)
              |3 -> (3,1)
              |4 -> (2,0)
              |5 -> (1,0)
              |_ -> failwith "OutOfBoundsException" in
    (r+x,c+y)


let string_to_char_list s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


type color =
  | Red | Blue | White | Orange
