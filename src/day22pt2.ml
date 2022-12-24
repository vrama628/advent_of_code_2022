open Core

type direction =
  | Up
  | Down
  | Left
  | Right
[@@deriving enumerate]

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

type dimension =
  | X
  | Y
  | Z
[@@deriving ord, sexp_of, eq]

type direction_3d = dimension * bool [@@deriving ord, sexp_of, eq]

let negate = Tuple2.map_snd ~f:not

let rec cross a b =
  match a, b with
  | (X, _), (X, _) | (Y, _), (Y, _) | (Z, _), (Z, _) ->
    raise (Invalid_argument "parallel")
  | (X, x), (Y, y) -> Z, Bool.(x = y)
  | (Y, y), (Z, z) -> X, Bool.(y = z)
  | (Z, z), (X, x) -> Z, Bool.(z = x)
  | _ -> negate (cross b a)

let rotation normal ((dim1, pol1) as dir1) ((dim2, pol2) as dir2) =
  if equal_dimension dim1 dim2 then
    if Bool.equal pol1 pol2 then
      []
    else
      [TurnRight; TurnRight]
  else if equal_direction_3d normal (cross dir1 dir2) then
    [TurnLeft]
  else
    [TurnRight]

type face_and_orientation = {
  face : direction_3d;
  orientation : direction_3d;
}
[@@deriving ord, sexp_of]

type face_mapping = {
  sector : int * int;
  face_and_orientation : face_and_orientation;
}
[@@deriving ord, sexp_of]

let rotate_3d { face; orientation } = function
  | Up -> { face = orientation; orientation = negate face }
  | Down -> { orientation = face; face = negate orientation }
  | Left -> { face = cross face orientation; orientation }
  | Right -> { face = cross orientation face; orientation }

module FMCmp = struct
  type t = face_mapping
  include Comparator.Make (struct
    type t = face_mapping
    let compare = compare_face_mapping
    let sexp_of_t = sexp_of_face_mapping
  end)
end

module TupleCmp = Tuple.Comparator (Int) (Int)

let () =
  let board, path =
    let lines = In_channel.input_lines In_channel.stdin in
    ( List.foldi
        (List.take_while lines ~f:(Fn.non String.is_empty))
        ~init:(Map.empty (module TupleCmp))
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
  let side_length = Map.length board / 6 |> Z.of_int |> Z.sqrt |> Z.to_int in
  let face_mappings =
    let sectors =
      Map.fold
        board
        ~init:(Set.empty (module TupleCmp))
        ~f:(fun ~key:(i, j) ~data:_ acc ->
          Set.add acc (i / side_length, j / side_length)
        )
    in
    let adjacent sector other =
      List.find
        all_of_direction
        ~f:Poly.(fun direction -> move direction sector = other)
    in
    let rec assign_mappings init =
      if Set.length init = 6 then
        init
      else
        Set.fold sectors ~init ~f:(fun mappings unmapped ->
            match
              Set.find_map mappings ~f:(fun mapping ->
                  Option.map
                    (adjacent mapping.sector unmapped)
                    ~f:(Tuple2.create mapping)
              )
            with
            | None -> mappings
            | Some (adjacent_mapping, adjacency_direction) ->
              let sector = unmapped in
              let face_and_orientation =
                rotate_3d
                  adjacent_mapping.face_and_orientation
                  adjacency_direction
              in
              Set.add mappings { sector; face_and_orientation }
        )
        |> assign_mappings
    in
    assign_mappings
      (Set.singleton
         (module FMCmp)
         {
           face_and_orientation = { face = Z, true; orientation = Y, true };
           sector = Set.choose_exn sectors;
         }
      )
  in
  let wrap direction (i, j) =
    let offset =
      match direction with
      | Up -> j % side_length
      | Down -> side_length - (j % side_length) - 1
      | Left -> side_length - (i % side_length) - 1
      | Right -> i % side_length
    in
    let { face_and_orientation; _ } =
      Set.find_exn
        face_mappings
        ~f:
          Poly.(fun { sector; _ } -> sector = (i / side_length, j / side_length))
    in
    let current_next_face_and_orientation =
      rotate_3d face_and_orientation direction
    in
    let wrapped_face =
      Set.find_exn
        face_mappings
        ~f:(fun { face_and_orientation = { face; _ }; _ } ->
          equal_direction_3d face current_next_face_and_orientation.face
      )
    in
    let new_direction =
      List.fold
        (rotation
           current_next_face_and_orientation.face
           wrapped_face.face_and_orientation.orientation
           current_next_face_and_orientation.orientation
        )
        ~init:direction
        ~f:(Fn.flip turn_direction)
    in
    let sector_i, sector_j =
      wrapped_face.sector |> Tuple2.map ~f:(( * ) side_length)
    in
    let new_coords =
      match new_direction with
      | Up -> sector_i + side_length - 1, sector_j + offset
      | Down -> sector_i, sector_j + side_length - offset - 1
      | Left -> sector_i + side_length - offset - 1, sector_j + side_length - 1
      | Right -> sector_i + offset, sector_j
    in
    new_direction, new_coords
  in
  let rec follow_path path pos direction =
    match path with
    | [] -> pos, direction
    | Forward 0 :: path -> follow_path path pos direction
    | Forward n :: path ->
      let next_direction, next_pos =
        if Map.mem board (move direction pos) then
          direction, move direction pos
        else
          wrap direction pos
      in
      if Map.find_exn board next_pos then
        follow_path (Forward (n - 1) :: path) next_pos next_direction
      else
        follow_path path pos direction
    | Turn turn :: path -> follow_path path pos (turn_direction turn direction)
  in
  let start_j =
    Map.fold board ~init:Int.max_value ~f:(fun ~key:(i, j) ~data:_ acc ->
        if i = 0 then
          min acc j
        else
          acc
    )
  in
  let (i, j), direction = follow_path path (0, start_j) Right in
  printf "%d\n" ((1000 * (i + 1)) + (4 * (j + 1)) + int_of_direction direction)
