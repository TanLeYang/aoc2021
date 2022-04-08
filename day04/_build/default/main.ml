type grid = {num: int; marked: bool}

let create_board num_rows num_cols = Array.make_matrix num_rows num_cols {num = 0; marked = false}

let transpose m =
  let num_rows = Array.length m in
  let num_cols = Array.length m.(0) in

  let transposed = create_board num_rows num_cols in
  for r = 0 to num_rows-1 do
    for c = 0 to num_cols-1 do
      Array.set transposed.(r) c m.(c).(r)
    done;
  done;

  transposed

let mark board num =
  let num_rows = Array.length board in
  let num_cols = Array.length board.(0) in
  for i = 0 to num_rows-1 do
    for j = 0 to num_cols-1 do
      if board.(i).(j).num = num then board.(i).(j) <- {num = num; marked = true}
    done;
  done;

  board

let rec run_round boards num =
  match boards with
    | [] -> boards
    | board :: remaining_boards ->
      let marked_board = mark board num in
      marked_board :: run_round remaining_boards num

let did_win board =
  let is_marked = fun g -> g.marked in
  Array.exists (Array.for_all is_marked) board
  || Array.exists (Array.for_all is_marked) (transpose board)

let winners_losers boards =
  let winners = List.filter did_win boards in
  let losers = List.filter (fun board -> not (did_win board)) boards in

  (winners, losers)

let calculate_score winning_board winning_num =
  let sum_of_unmarked = Array.fold_left
    (fun total row ->
      Array.fold_left (
        fun acc grid -> if grid.marked then acc else acc + grid.num
      ) total row)
    0 winning_board in

  sum_of_unmarked * winning_num

let rec run_game boards numbers =
  match numbers with
    | [] -> failwith "No winners after all numbers have been used"
    | num :: remaining_nums ->
      let new_boards = run_round boards num in
      let (winners, losers) = winners_losers new_boards in
      match winners with
        | [] -> run_game losers remaining_nums
        | first_winner :: _ -> calculate_score first_winner num

let rec find_last_winner boards numbers prev_winner prev_num =
  match boards with
    | [] -> (prev_winner, prev_num)
    | boards ->
      match numbers with
        | [] -> (prev_winner, prev_num)
        | num :: remaining_nums ->
          let new_boards = run_round boards num in
          let (winners, losers) = winners_losers new_boards in
          match winners with
            | [] -> find_last_winner losers remaining_nums prev_winner prev_num
            | winners ->
              find_last_winner losers remaining_nums (List.nth_opt (List.rev winners) 0) num

let parse_input num_rows num_cols =
  let ic = open_in "input" in

  let numbers = input_line ic
    |> String.split_on_char ','
    |> List.map int_of_string
  in

  let rec parse_boards boards =
    try
      let () = match input_line ic with
        | _   -> ()
      in

      let board = create_board num_rows num_cols in

      for i = 0 to num_rows-1 do
        let empty_row = Array.get board i in

        input_line ic
        |> String.split_on_char ' '
        |> List.filter ((<>) "")
        |> List.map int_of_string
        |> List.iteri (fun idx num -> empty_row.(idx) <- {num = num; marked = false})
      done;

      parse_boards (board :: boards)

    with
    | End_of_file ->
      let () = close_in ic in
      List.rev boards
  in

  let boards = parse_boards [] in
  (numbers, boards)

let () =
  (* part 1*)
  let num_rows = 5 in
  let num_cols = 5 in
  let (numbers, boards) = parse_input num_rows num_cols in
  let final_score = run_game boards numbers in
  print_endline (string_of_int final_score)

let () =
  (* part 2*)
  let num_rows = 5 in
  let num_cols = 5 in
  let (numbers, boards) = parse_input num_rows num_cols in
  let last_winner = find_last_winner boards numbers None 0 in
  match last_winner with
    | (None, _) -> failwith "No winner found"
    | (Some winner, winning_num) -> print_endline (string_of_int (calculate_score winner winning_num))