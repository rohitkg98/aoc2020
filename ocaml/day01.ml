let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done ;
    !lines
  with End_of_file -> close_in chan ; List.rev !lines

module Ints = Set.Make(Int)

let set_of_list xs =
  List.fold_right Ints.add xs Ints.empty

let build_set_of_ints list_of_strs =
  List.map int_of_string list_of_strs
  |> set_of_list

let find_pairs_of_sum sum set =
  let check_if_mem remaining = 
    Ints.mem remaining set
  in
  Ints.filter (fun x -> check_if_mem (sum - x)) set
  |> Ints.elements

let find_triplets_of_sum sum set = 
  let get_all_possible_pairs x =
    find_pairs_of_sum (sum - x) set
  in
  List.map get_all_possible_pairs (Ints.elements set)
  |> List.filter (fun x -> x = [] |> not)

let print_val_and_compliment sum x =
  Printf.sprintf "%d %d; product: %d\n" x (sum - x) (x * (sum - x))
  |> print_string

let print_triplet sum = function
  | [x1; x2] -> let rem = (sum - x1 - x2) in
                  Printf.sprintf "%d %d %d; product: %d\n" x1 x2 rem (x1 * x2 * rem)
                  |> print_string
  | _ -> raise Exit

let run_01 =
  read_file "../inputs/day01.txt"
  |> build_set_of_ints
  |> find_pairs_of_sum 2020
  |> List.iter (print_val_and_compliment 2020)

let run_02 = 
  read_file "../inputs/day01.txt"
  |> build_set_of_ints
  |> find_triplets_of_sum 2020
  |> List.iter (print_triplet 2020)
