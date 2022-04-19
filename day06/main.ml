let reproduction_interval = 7
let new_lanternfish_delay = 2

let (mod) x y = ((x mod y) + y) mod y

let population_after (days: int) (lanternfishes: int list): int =
  let arr = Array.make (reproduction_interval + new_lanternfish_delay) 0 in

  List.iter (fun lf -> Array.set arr lf (arr.(lf) + 1)) lanternfishes;

  for _ = 1 to days do
    let new_arr = Array.make (reproduction_interval + new_lanternfish_delay) 0 in

    for j = 0 to (reproduction_interval + new_lanternfish_delay - 1) do
      let new_idx = if j < reproduction_interval
        then (j - 1) mod reproduction_interval else
        j - 1
      in
      let () = new_arr.(new_idx) <- new_arr.(new_idx) + arr.(j) in
      if j = 0 then new_arr.(reproduction_interval + new_lanternfish_delay - 1) <- arr.(j)
    done;

    for j = 0 to (reproduction_interval + new_lanternfish_delay - 1) do
      arr.(j) <- new_arr.(j)
    done;
  done;

  Array.fold_left (+) 0 arr

let parse_input (input_file: string): int list =
  let ic = open_in input_file in
  input_line ic
  |> String.split_on_char ','
  |> List.map int_of_string

let () =
  let initial_fishes = parse_input "input.txt" in
  print_int (population_after 256 initial_fishes)