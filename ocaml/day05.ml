let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

type seat = { row : int; column : int }

let id_of_seat { row; column } = (row * 8) + column

type interval = { min : int; max : int }

type step = Up | Down

let narrow_down { min; max } = function
  | Up -> { min; max = min + ((max - min) / 2) }
  | Down -> { min = min + ((max - min) / 2) + 1; max }

let traverse interval steps = List.fold_left narrow_down interval steps

let map_to_step up down = function
  | x when x = up -> Up
  | x when x = down -> Down
  | _ -> raise Exit

let step_of_row = map_to_step 'F' 'B'

let step_of_col = map_to_step 'L' 'R'

let build_find converter interval steps =
  let result =
    String.to_seq steps |> List.of_seq |> List.map converter
    |> traverse interval
  in
  result.min

let find_row = build_find step_of_row { min = 0; max = 127 }

let find_col = build_find step_of_col { min = 0; max = 7 }

let find_seat seat =
  let len = String.length seat in
  let row = String.sub seat 0 (len - 3) and col = String.sub seat (len - 3) 3 in
  { row = find_row row; column = find_col col }

let ( >> ) f g x = g (f x)

let run_01 =
  read_file "../inputs/day05.txt"
  |> List.map (find_seat >> id_of_seat)
  |> List.fold_left (fun acc x -> if x > acc then x else acc) 0
  |> print_int;
  print_newline ()

let rec find_missing = function
  | hd :: next :: _ when next - hd != 1 -> hd + 1
  | _ :: tl -> find_missing tl
  | _ -> raise Exit

let run_02 =
  read_file "../inputs/day05.txt"
  |> List.map (find_seat >> id_of_seat)
  |> List.sort Stdlib.compare |> find_missing |> print_int;
  print_newline ()
