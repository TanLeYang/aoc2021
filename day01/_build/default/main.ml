let rec build_list l ic = 
  match input_line ic with 
    | line -> build_list (int_of_string line :: l) ic
    | exception End_of_file -> close_in ic; List.rev l

let increasing_count l =
  let rec increasing_count_aux acc = function
    | h1 :: (h2 :: _ as t) -> 
      if h1 < h2 then increasing_count_aux (acc + 1) t
      else increasing_count_aux acc t 
    | _ -> acc in
 
  increasing_count_aux 0 l

let increasing_window_count l = 
  let rec increasing_window_count_aux acc = function
    | h1 :: (_ :: _ :: h4 :: _ as t) ->
      if h1 < h4 then increasing_window_count_aux (acc + 1) t
      else increasing_window_count_aux acc t
    | _ -> acc in

  increasing_window_count_aux 0 l

let () = 
  let ic = open_in "input" in
  let input_lst = build_list [] ic in
  let result_1 = increasing_count input_lst in
  let result_2 = increasing_window_count input_lst in
  print_endline (string_of_int result_1);
  print_endline (string_of_int result_2)
