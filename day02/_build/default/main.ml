type direction = Forward | Down | Up

let direction_of_str = function
  | "forward" -> Forward
  | "down" -> Down
  | "up" -> Up
  | _ -> failwith "No matching direction"

type move = {direction : direction; magnitude : int}
type position = {horizontal: int; depth: int; aim: int}

let rec build_move_lst move_lst ic =
  match input_line ic with
    | line ->
      let parts = String.split_on_char ' ' line in
      begin match parts with 
        | h1 :: h2 :: [] ->
          let move = {direction = direction_of_str h1; magnitude = int_of_string h2} in   
          build_move_lst (move :: move_lst) ic
        | _ -> failwith "Invalid input"
      end
    | exception End_of_file -> close_in ic; List.rev move_lst

let rec final_position_product {horizontal = horizontal; depth = depth; aim = aim} move_lst = match move_lst with
  | [] -> horizontal * depth
  | {direction = direction; magnitude = magnitude} :: t ->
    let new_position = match direction with
      | Forward -> {horizontal = horizontal + magnitude; depth = depth + aim * magnitude; aim = aim}
      | Down -> {horizontal = horizontal; depth = depth; aim = aim + magnitude}
      | Up -> {horizontal = horizontal; depth = depth; aim = aim - magnitude} in 
    final_position_product new_position t

let () = 
    let ic = open_in "input.txt" in
    let move_lst = build_move_lst [] ic in
    let result1 = final_position_product {horizontal = 0; depth = 0; aim = 0} move_lst in
    print_endline (string_of_int result1)