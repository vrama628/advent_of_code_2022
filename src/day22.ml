open Core

type direction =
  | Up
  | Down
  | Left
  | Right

let int_of_direction = function Right -> 0 | Down -> 1 | Left -> 2 | Up -> 3

type turn =
  | TurnLeft
  | TurnRight

let turn_direction turn direction =
  match turn, direction with
  | TurnRight, Left | TurnLeft, Right -> Up
  | TurnRight, Right | TurnLeft, Left -> Down
  | TurnRight, Down | TurnLeft, Up -> Left
  | TurnRight, Up | TurnLeft, Down -> Right

let turn_of_char = function
  | 'L' -> TurnLeft
  | 'R' -> TurnRight
  | _ -> failwith "unreachable"

type path =
  | Forward of int
  | Turn of turn

let rec parse_path str =
  if String.is_empty str then
    []
  else if Char.is_alpha str.[0] then
    Turn (turn_of_char str.[0]) :: parse_path (String.drop_prefix str 1)
  else
    let forward = String.take_while str ~f:Char.is_digit in
    Forward (int_of_string forward)
    :: parse_path (String.drop_prefix str (String.length forward))

let move direction (i, j) =
  match direction with
  | Up -> i - 1, j
  | Down -> i + 1, j
  | Left -> i, j - 1
  | Right -> i, j + 1

let () =
  let board, path =
    let lines = In_channel.input_lines In_channel.stdin in
    ( List.foldi
        (List.take_while lines ~f:(Fn.compose not String.is_empty))
        ~init:(Map.empty (module Tuple.Comparator (Int) (Int)))
        ~f:(fun i init ->
          String.foldi ~init ~f:(fun j acc c ->
              match c with
              | '.' -> Map.add_exn acc ~key:(i, j) ~data:true
              | '#' -> Map.add_exn acc ~key:(i, j) ~data:false
              | _ -> acc
          )
        ),
      parse_path (List.last_exn lines) )
  in
  let min_row, min_col, max_row, max_col =
    Map.fold
      board
      ~init:(Int.max_value, Int.max_value, Int.min_value, Int.min_value)
      ~f:(fun ~key:(i, j) ~data:_ (min_i, min_j, max_i, max_j) ->
        min i min_i, min j min_j, max i max_i, max j max_j
    )
  in
  let first direction cross_coord =
    let rec loop pos =
      if Map.mem board pos then
        pos
      else
        loop (move direction pos)
    in
    let pos =
      match direction with
      | Up -> max_row, cross_coord
      | Down -> min_row, cross_coord
      | Left -> cross_coord, max_col
      | Right -> cross_coord, min_col
    in
    loop pos
  in
  let cross = function Up | Down -> snd | Left | Right -> fst in
  let rec follow_path path pos direction =
    match path with
    | [] -> pos, direction
    | Forward 0 :: path -> follow_path path pos direction
    | Forward n :: path ->
      let next_pos =
        if Map.mem board (move direction pos) then
          move direction pos
        else
          first direction (cross direction pos)
      in
      if Map.find_exn board next_pos then
        follow_path (Forward (n - 1) :: path) next_pos direction
      else
        follow_path path pos direction
    | Turn turn :: path -> follow_path path pos (turn_direction turn direction)
  in
  let (i, j), direction = follow_path path (first Right 0) Right in
  printf "%d\n" ((1000 * (i + 1)) + (4 * (j + 1)) + int_of_direction direction)
