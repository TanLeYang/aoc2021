let calculate_const_cost (initial_positions: int list) (destination: int): int =
  List.map (fun x -> abs(destination - x))  initial_positions
  |> List.fold_left (+) 0

let calculate_linear_cost (initial_positions: int list) (destination: int): int =
  List.map (fun x ->
    let n = abs(destination - x) in
    n * (n + 1) / 2
  ) initial_positions
  |> List.fold_left (+) 0

let cheapest_outcome (cost_fn: int list -> int -> int) (positions: int list): int =
  let max_position = List.fold_left Stdlib.max min_int positions in
  let costs =
    List.init (max_position + 1) (fun x -> x)
    |> List.map (fun destination -> cost_fn positions destination)
  in

  List.fold_left Stdlib.min (max_int) costs

let parse_input (input_file: string): int list =
  let ic = open_in input_file in
  input_line ic
  |> String.split_on_char ','
  |> List.map int_of_string

let () =
  let initial_positions = parse_input "input.txt" in

  (* part 1*)
  cheapest_outcome calculate_const_cost initial_positions
  |> print_int;

  print_newline();

  (* part 2*)
  cheapest_outcome calculate_linear_cost initial_positions
  |> print_int;