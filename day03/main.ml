open Core

let to_bits str = 
  List.map (String.to_list str) ~f:(fun c -> if Char.equal c '0' then 0 else 1)

let to_int_vals bits =
  List.map bits ~f:(fun i -> if i = 0 then -1 else 1)

let input = 
  In_channel.read_lines "input"
  |> List.map ~f:to_bits 

let int_values = List.map input ~f:to_int_vals

let combine_lst lst1 lst2 = List.map2_exn lst1 lst2 ~f: (fun a b -> a + b) 

let sum_int_values input = match input with
  | [] -> failwith "Empty input"
  | h :: t -> List.fold_left t ~init:h ~f:combine_lst

let gamma_bit int_val = if int_val > 0 then 1 else 0
let epsilon_bit int_val = if int_val < 0 then 1 else 0

let decimal_of_bits bits = 
  let rec aux acc bits = match bits with 
    | [] -> acc
    | h :: t -> 
      let exponent = (List.length bits) - 1 in
      aux (acc + h * (Int.pow 2 exponent)) t 
  in aux 0 bits

let oxygen_filter int_values input_number n = 
  if (List.nth_exn int_values n) < 0 then (List.nth_exn input_number n) = 0
  else (List.nth_exn input_number n) = 1  

let co2_filter int_values input_number n =
  if (List.nth_exn int_values n) < 0 then (List.nth_exn input_number n) = 1
  else (List.nth_exn input_number n) = 0

let filter_numbers int_values input_numbers filter =
  let rec aux int_values input_numbers n =
    match input_numbers with
      | [] -> failwith "Filtered to zero elements"
      | [ x ] -> x 
      | h :: _ ->
        if List.length h <= n then failwith "Unable to filter to 1 number"
        else 
          let filtered = List.filter input_numbers ~f:(fun i -> filter int_values i n) in
          let new_int_values = sum_int_values (List.map filtered ~f:to_int_vals) in
          aux new_int_values filtered (n + 1)

  in aux int_values input_numbers 0

let () = 
  let int_values = sum_int_values int_values in
  let gamma_value = decimal_of_bits (List.map int_values ~f:gamma_bit) in
  let epsilon_value = decimal_of_bits (List.map int_values ~f:epsilon_bit) in
  print_endline (string_of_int (gamma_value * epsilon_value));
  let oxygen_val = decimal_of_bits (filter_numbers int_values input oxygen_filter) in
  let co2_val = decimal_of_bits (filter_numbers int_values input co2_filter) in
  print_endline (string_of_int (oxygen_val * co2_val))
