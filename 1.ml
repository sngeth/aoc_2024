let sum_abs_differences lst =
  let lefts, rights = List.split lst in
  let sorted_lefts = List.sort compare lefts in
  let sorted_rights = List.sort compare rights in
  let sorted_pairs = List.combine sorted_lefts sorted_rights in

  List.fold_left 
    (fun acc (x, y) -> acc + abs(x - y))
    0 
    sorted_pairs

let read_pairs_from_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      let x, y = Scanf.sscanf line "%d %d" (fun x y -> (x, y)) in
      read_lines ((x, y) :: acc)
    with End_of_file ->
      close_in ic;
      acc
  in
  read_lines []

let () =
  let pairs = read_pairs_from_file "input.txt" in
  let result = sum_abs_differences pairs in
  Printf.printf "The sum of absolute differences is: %d\n" result
