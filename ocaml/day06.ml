let read_file filename =
  let lines = ref [ "" ] in
  let chan = open_in filename in
  try
    while true do
      let line = input_line chan in
      if line = "" then lines := "" :: !lines
      else lines := (line ^ " " ^ List.hd !lines) :: List.tl !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

module Questions = Set.Make (Char)

let form_set yes_ans =
  String.to_seq yes_ans
  |> Seq.fold_left (fun acc x -> Questions.add x acc) Questions.empty
  |> Questions.remove ' '

let count_unique yes_ans =
  form_set yes_ans
  |> Questions.cardinal

let run_01 =
  read_file "../inputs/day06.txt"
  |> List.map count_unique |> List.fold_left ( + ) 0
  |> print_int;
  print_newline ()

let fold_left1 f l = List.fold_left f (List.hd l) l

let count_present_in_all yes_ans =
  String.trim yes_ans
  |> String.split_on_char ' '
  |> List.map form_set
  |> fold_left1 Questions.inter  
  |> Questions.cardinal

let run_02 =
  read_file "../inputs/day06.txt"
  |> List.map count_present_in_all |> List.fold_left ( + ) 0
  |> print_int;
  print_newline ()
