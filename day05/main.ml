type point = {
  x: int;
  y: int
}

type segment = {
  start_pt: point;
  end_pt: point
}

type linesegment =
  | Horizontal of segment
  | Vertical of segment
  | Diagonal of segment

let is_horizontal (s: segment): bool = s.start_pt.y = s.end_pt.y
let is_vertical (s: segment): bool = s.start_pt.x = s.end_pt.x

let type_of_linesegment (s: segment): linesegment =
  if is_horizontal s then Horizontal s
  else if is_vertical s then Vertical s
  else Diagonal s

let rec get_points_on_axis_aligned (variable_pt: point) (fixed_pt: point) (next: point->point): point list =
  if variable_pt = fixed_pt then [variable_pt]
  else variable_pt :: get_points_on_axis_aligned (next variable_pt) fixed_pt next

let get_points_on_horizontal (l: segment): point list =
  let next_f = fun p -> {x = p.x+1; y = p.y} in
  if l.start_pt.x < l.end_pt.x then get_points_on_axis_aligned l.start_pt l.end_pt next_f
  else get_points_on_axis_aligned l.end_pt l.start_pt next_f

let get_points_on_vertical (l: segment): point list =
  let next_f = fun p -> {x = p.x; y = p.y+1} in
  if l.start_pt.y < l.end_pt.y then get_points_on_axis_aligned l.start_pt l.end_pt next_f
  else get_points_on_axis_aligned l.end_pt l.start_pt next_f

let get_points_on_diagonal (l: segment): point list =
  let dx x = if l.start_pt.x < l.end_pt.x then x+1 else x-1 in
  let dy y = if l.start_pt.y < l.end_pt.y then y+1 else y-1 in

  let rec points_on_diagonal variable_pt fixed_pt delta_x delta_y =
    if variable_pt = fixed_pt then [variable_pt]
    else
      let new_pt = {x=delta_x variable_pt.x; y=delta_y variable_pt.y} in
      variable_pt :: points_on_diagonal new_pt fixed_pt delta_x delta_y
  in

  points_on_diagonal l.start_pt l.end_pt dx dy

let points_on_line (ls: linesegment): point list = match ls with
  | Horizontal s -> get_points_on_horizontal s
  | Vertical s -> get_points_on_vertical s
  | Diagonal s -> get_points_on_diagonal s

let rec number_of_overlapping_points
  (found_point: point -> unit) (compute_result: unit -> int) (linesegments: linesegment list): int =
  match linesegments with
    | [] -> compute_result()
    | ls :: remaining ->
      let () = points_on_line ls
        |> List.iter found_point in
      number_of_overlapping_points found_point compute_result remaining

let parse_input (input_file: string): linesegment list =
  let ic = open_in input_file in

  let string_to_point (s: string): point =
    let parts = String.split_on_char ',' s
    |> List.map int_of_string in

    {x=List.nth parts 0; y=List.nth parts 1}
  in

  let rec parse_linesegments linesegments =
    try
      let points = input_line ic
      |> String.split_on_char ' '
      |> List.filter ((<>) "->")
      |> List.map string_to_point
      in

      let ls = type_of_linesegment {start_pt=List.nth points 0; end_pt=List.nth points 1} in
      parse_linesegments (ls :: linesegments)

    with
    | End_of_file ->
      let () = close_in ic in
      linesegments
  in

  parse_linesegments []

let () =
  let ht = Hashtbl.create 64 in

  let found_pt (pt: point): unit =
    match Hashtbl.find_opt ht pt with
      | None -> Hashtbl.add ht pt 1
      | Some count ->
        let () = Hashtbl.remove ht pt in
        Hashtbl.add ht pt (count+1)
  in

  let num_overlapping (): int =
    Hashtbl.fold (fun _ v acc -> if v > 1 then acc + 1 else acc) ht 0
  in

  (* part 1 *)
  let answer = parse_input "input.txt"
  |> List.filter (fun ls ->
      match ls with
        | Horizontal _  -> true
        | Vertical _ -> true
        | Diagonal _ -> false
    )
  |> number_of_overlapping_points found_pt num_overlapping in

  print_int answer;
  print_endline "";

  Hashtbl.clear ht;

  (* part 2*)
  let answer = parse_input "input.txt"
  |> number_of_overlapping_points found_pt num_overlapping in

  print_int answer;