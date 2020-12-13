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

let int_of_tree tree = if tree = '#' then 1 else 0

let next_index curr length right = (curr + right) mod length

let skip_n n xs = List.filteri (fun index _ -> index mod n = 0) xs

let transform trees (right, down) =
  let length = List.hd trees |> String.length in
  let acc (index, count) row =
    (next_index index length right, count + int_of_tree row.[index])
  in
  skip_n down trees |> List.fold_left acc (0, 0)

let () =
  let trees = read_file "../inputs/day03.txt" in
  let slopes = [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ] in
  List.map (fun slope -> transform trees slope) slopes
  |> List.fold_left (fun acc (_, count) -> acc * count) 1
  |> print_int;
  print_newline ()
