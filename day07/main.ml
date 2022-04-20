let calculate_cost (initial_positions: int list) (destination: int): int =
  List.map (fun x -> abs(destination - x))  initial_positions
  |> List.fold_left (+) 0

let cheapest_outcome (positions: int list): int =
  let max_position = List.fold_left Stdlib.max 0 positions in
  let costs =
    List.init (max_position + 1) (fun x -> x)
    |> List.map (fun destination -> calculate_cost positions destination)
  in

  List.fold_left Stdlib.min ((max_position + 1) * (List.length positions)) costs

let parse_input (input_file: string): int list =
  let ic = open_in input_file in
  input_line ic
  |> String.split_on_char ','
  |> List.map int_of_string

let () =
  parse_input "input.txt"
  |> cheapest_outcome
  |> print_int