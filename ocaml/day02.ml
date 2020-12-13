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

type policy = { min : int; max : int; letter : char; password : string }

let is_policy_valid { min; max; letter; password } =
  let len =
    String.to_seq password
    |> Seq.filter (( = ) letter)
    |> String.of_seq |> String.length
  in
  len >= min && len <= max

let is_new_policy_valid { min; max; letter; password } =
  let first = password.[min - 1] and second = password.[max - 1] in
  first != second && (first = letter || second = letter)

let filter_valid_passwords validator policies = List.filter validator policies

let policy_of_string str =
  Scanf.sscanf str "%d-%d %c: %s" (fun min max letter password ->
      { min; max; letter; password })

let parse_policies str_policies = List.map policy_of_string str_policies

let run_with_policy validator =
  let nvalid =
    read_file "../inputs/day02.txt"
    |> parse_policies
    |> filter_valid_passwords validator
    |> List.length
  in
  print_int nvalid;
  print_newline ()

let run_01 = run_with_policy is_policy_valid

let run_02 = run_with_policy is_new_policy_valid
