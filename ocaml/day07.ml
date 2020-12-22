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

let ( >> ) f g x = g (f x)

type bag = { color : string; count : int }

type rule = { parent : bag list; current : bag; children : bag list }

module Rule = struct
  let add_parent new_parent { parent; current; children } =
    { parent = new_parent :: parent; current; children }

  let add_child new_child { parent; current; children } =
    { parent; current; children = new_child :: children }

  let merge_two one two =
    if one.current.color <> two.current.color then raise Exit
    else
      let current = one.current in
      {
        parent = one.parent @ two.parent;
        current;
        children = one.children @ two.children;
      }
end

let rec join_n n str_list =
  if n = 1 then List.hd str_list
  else List.hd str_list ^ " " ^ join_n (n - 1) (List.tl str_list)

let parse_parent str =
  let tokens = String.split_on_char ' ' str in
  join_n 2 tokens |> fun color -> { color; count = 1 }

let parse_color_and_count str =
  let tokens = String.split_on_char ' ' str in
  join_n 2 (List.tl tokens) |> fun color ->
  { color; count = int_of_string (List.hd tokens) }

let begins_with to_check str =
  Str.first_chars str (String.length to_check) |> ( = ) to_check

let parse_children str =
  if begins_with "no" str then []
  else
    String.split_on_char ',' str
    |> List.map (String.trim >> parse_color_and_count)

let string_split sub_str = Str.split (Str.regexp sub_str)

let rule_of_str str =
  (* String can be of 3 types:
      if no bags then: no other bags
      if one bag: `count` `adjective` `color` bag.
      if more than one bag: same format as above, but comma separated
  *)
  let bags = string_split " contain " str in
  let parent = List.hd bags and children = List.nth bags 1 in
  {
    parent = [];
    current = parse_parent parent;
    children = parse_children children;
  }

module RuleMap = Map.Make (String)

let insert_into map rule =
  let color = rule.current.color in
  try
    let older = RuleMap.find color map in
    RuleMap.add color (Rule.merge_two older rule) map
  with Not_found -> RuleMap.add color rule map

let inject_as_parent rule map =
  let parent = rule.current in
  let form_new child =
    { parent = [ parent ]; current = child; children = [] }
  in
  List.fold_left (fun acc x -> form_new x |> insert_into acc) map rule.children

let full_insert map rule = insert_into map rule |> inject_as_parent rule

module RuleSet = Set.Make (String)

let count_all_parents color map =
  let rec find_all color set =
    let rule = RuleMap.find color map in
    List.fold_left
      (fun acc clr -> find_all clr.color acc |> RuleSet.add clr.color)
      set rule.parent
  in
  find_all color RuleSet.empty |> RuleSet.cardinal

let run_01 =
  read_file "../inputs/day07.txt"
  |> List.map rule_of_str
  |> List.fold_left full_insert RuleMap.empty
  |> count_all_parents "shiny gold"
  |> print_int;
  print_newline ()

let rec count_all_children color map =
  let current = RuleMap.find color map in
  List.fold_left
    (fun acc x -> acc + x.count + (x.count * (count_all_children x.color map)))
    0 current.children

let run_02 =
  read_file "../inputs/day07.txt"
  |> List.map rule_of_str
  |> List.fold_left full_insert RuleMap.empty
  |> count_all_children "shiny gold"
  |> print_int;
  print_newline ()
