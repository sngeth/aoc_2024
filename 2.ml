#load "str.cma";;

(*Input*)
(*7 6 4 2 1*)
(*1 2 7 8 9*)
(*9 7 6 2 1*)
(*1 3 2 4 5*)
(*8 6 4 4 1*)
(*1 3 6 7 9*)
(*This example data contains six reports each containing five levels.*)
(**)
(*The engineers are trying to figure out which reports are safe. The Red-Nosed reactor safety systems can only tolerate levels that are either gradually increasing or gradually decreasing. So, a report only counts as safe if both of the following are true:*)
(**)
(*The levels are either all increasing or all decreasing.*)
(*Any two adjacent levels differ by at least one and at most three.*)
(*In the example above, the reports can be found safe or unsafe by checking those rules:*)
(**)
(*7 6 4 2 1: Safe because the levels are all decreasing by 1 or 2.*)
(*1 2 7 8 9: Unsafe because 2 7 is an increase of 5.*)
(*9 7 6 2 1: Unsafe because 6 2 is a decrease of 4.*)
(*1 3 2 4 5: Unsafe because 1 3 is increasing but 3 2 is decreasing.*)
(*8 6 4 4 1: Unsafe because 4 4 is neither an increase or a decrease.*)
(*1 3 6 7 9: Safe because the levels are all increasing by 1, 2, or 3.*)
(*So, in this example, 2 reports are safe.*)

let read_numbers_from_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      let numbers = List.map int_of_string (Str.split (Str.regexp " +") line) in
      read_lines (numbers :: acc)
    with End_of_file ->
      close_in ic;
      acc
  in
  read_lines []

let is_safe_line numbers =
  let rec check_increasing = function
    | [] | [_] -> true
    | x :: y :: rest -> (y > x && y - x <= 3) && check_increasing (y :: rest)
  in
  let rec check_decreasing = function
    | [] | [_] -> true
    | x :: y :: rest -> (x > y && x - y <= 3) && check_decreasing (y :: rest)
  in
  check_increasing numbers || check_decreasing numbers

let () =
  let number_lists = read_numbers_from_file "input2.txt" in
  let safe_count = List.fold_left (fun acc numbers ->
    if is_safe_line numbers then acc + 1 else acc
  ) 0 number_lists in
  Printf.printf "Number of safe lines: %d\n" safe_count;
