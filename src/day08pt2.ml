open Core

let rec viewing_distance (grid : char array array) (di, dj) height (i, j) : int
    =
  if i < 0 || i >= Array.length grid || j < 0 || j >= Array.length grid.(0) then
    0
  else if Char.(grid.(i).(j) >= height) then
    1
  else
    1 + viewing_distance grid (di, dj) height (i + di, j + dj)

let scenic_score (grid : char array array) (i, j) =
  let height = grid.(i).(j) in
  viewing_distance grid (1, 0) height (i + 1, j)
  * viewing_distance grid (-1, 0) height (i - 1, j)
  * viewing_distance grid (0, 1) height (i, j + 1)
  * viewing_distance grid (0, -1) height (i, j - 1)

let summarize (grid : char array array) : int =
  List.cartesian_product
    (List.range 0 (Array.length grid))
    (List.range 0 (Array.length grid.(0)))
  |> List.map ~f:(scenic_score grid)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn

let rec loop (acc : char array list) : int =
  match In_channel.input_line In_channel.stdin with
  | None -> summarize (Array.of_list acc)
  | Some line -> loop (String.to_array line :: acc)

let () = Printf.printf "%d\n" (loop [])
