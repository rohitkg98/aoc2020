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

module Passport = Map.Make (String)

let parse_passport str =
  (* Split at colon *)
  let split_and_assoc passport key =
    let key_val = String.split_on_char ':' key in
    Passport.add (List.hd key_val) (List.nth key_val 1) passport
  in
  String.trim str |> String.split_on_char ' '
  |> List.fold_left split_and_assoc Passport.empty

let mandatory_keys = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]

let old_is_passport_valid passport =
  List.fold_left
    (fun acc key -> Passport.mem key passport && acc)
    true mandatory_keys

let run_01 =
  read_file "../inputs/day04.txt"
  |> List.map parse_passport
  |> List.fold_left
       (fun acc x -> if old_is_passport_valid x then acc + 1 else acc)
       0
  |> print_int;
  print_newline ()

module Validators = Map.Make (String)

let ( >> ) f g x = g (f x)

let byr_v byr = byr >= 1920 && byr <= 2002

let iyr_v iyr = iyr >= 2010 && iyr <= 2020

let eyr_v eyr = eyr >= 2020 && eyr <= 2030

let hgt_v hgt =
  let cm_v cm = cm >= 150 && cm <= 193 and in_v inc = inc >= 59 && inc <= 76 in
  let len = String.length hgt in
  let prefix = String.sub hgt 0 (len - 2)
  and suffix = String.sub hgt (len - 2) 2 in
  (suffix = "cm" && cm_v (int_of_string prefix))
  || (suffix = "in" && in_v (int_of_string prefix))

let is_hex chr = (chr >= 'a' && chr <= 'f') || (chr >= '0' && chr <= '9')

let hcl_v hcl =
  hcl.[0] = '#'
  &&
  let len = String.length hcl in
  len = 7
  && String.to_seq (String.sub hcl 1 (len - 1))
     |> Seq.fold_left (fun acc x -> acc && is_hex x) true

module Colors = Set.Make (String)

let allowed_ecl =
  Colors.of_list [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]

let ecl_v ecl = Colors.mem ecl allowed_ecl

let pid_v pid =
  let len = String.length pid in
  len = 9
  &&
  try
    int_of_string pid |> ignore;
    true
  with Failure _ -> false

let validators =
  [
    ("byr", int_of_string >> byr_v);
    ("iyr", int_of_string >> iyr_v);
    ("eyr", int_of_string >> eyr_v);
    ("hgt", hgt_v);
    ("hcl", hcl_v);
    ("ecl", ecl_v);
    ("pid", pid_v);
  ]
  |> List.to_seq |> Validators.of_seq

let is_passport_valid passport =
  let validate acc key =
    Passport.mem key passport
    && (Validators.find key validators) (Passport.find key passport)
    && acc
  in
  List.fold_left validate true mandatory_keys

let run_02 =
  read_file "../inputs/day04.txt"
  |> List.map parse_passport
  |> List.fold_left
       (fun acc x -> if is_passport_valid x then acc + 1 else acc)
       0
  |> print_int;
  print_newline ()
