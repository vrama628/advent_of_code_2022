open Core

type rock = bool array array

let rocks : rock array =
  [|
    [| [| true; true; true; true |] |];
    [|
      [| false; true; false |]; [| true; true; true |]; [| false; true; false |];
    |];
    [|
      [| false; false; true |]; [| false; false; true |]; [| true; true; true |];
    |];
    [| [| true |]; [| true |]; [| true |]; [| true |] |];
    [| [| true; true |]; [| true; true |] |];
  |]

let height_of_rock : rock -> int = Array.length
let width_of_rock (rock : rock) : int = Array.length rock.(0)

let highest_nonempty_row_of_chamber chamber =
  match Array.findi chamber ~f:(Fn.const (Array.exists ~f:Fn.id)) with
  | Some (i, _) -> i
  | None -> Array.length chamber
let heighten_chamber n chamber =
  Array.append (Array.init n ~f:(fun _ -> Array.create ~len:7 false)) chamber
let lop_chamber chamber =
  if Array.length chamber < 200 then
    chamber, 0
  else
    Array.sub chamber ~pos:0 ~len:100, Array.length chamber - 100

let intersects rock chamber r c =
  c < 0
  || c + width_of_rock rock > 7
  || r + height_of_rock rock > Array.length chamber
  || Array.existsi rock ~f:(fun row_i row ->
         Array.existsi row ~f:(fun col_i elt ->
             elt && chamber.(r + row_i).(c + col_i)
         )
     )

let () =
  let jets =
    In_channel.input_line_exn In_channel.stdin
    |> String.to_array
    |> Array.map ~f:(function
           | '<' -> -1
           | '>' -> 1
           | _ -> failwith "unreachable"
           )
  in
  let rec loop rock jet chamber r c lopped =
    if Float.(Random.float 1. < 0.00001) then
      printf "\r%f%%%!" (100. *. float rock /. 1000000000000.);
    if rock >= 1000000000000 then
      Array.length chamber - highest_nonempty_row_of_chamber chamber + lopped
    else
      let r, c =
        if intersects rocks.(rock mod 5) chamber r (c + jets.(jet)) then
          r, c
        else
          r, c + jets.(jet)
      in
      if intersects rocks.(rock mod 5) chamber (r + 1) c then (
        Array.iteri
          rocks.(rock mod 5)
          ~f:(fun row_i row ->
            Array.iteri row ~f:(fun col_i elt ->
                chamber.(r + row_i).(c + col_i) <-
                  chamber.(r + row_i).(c + col_i) || elt
            )
          );
        let row =
          highest_nonempty_row_of_chamber chamber
          - 3
          - height_of_rock rocks.((rock + 1) mod 5)
        in
        let heighten_chamber_by = max 0 (-row) in
        let chamber = heighten_chamber heighten_chamber_by chamber in
        let chamber, just_lopped = lop_chamber chamber in
        loop
          (rock + 1)
          ((jet + 1) mod Array.length jets)
          chamber
          (row + heighten_chamber_by)
          2
          (lopped + just_lopped)
      ) else
        loop rock ((jet + 1) mod Array.length jets) chamber (r + 1) c lopped
  in
  printf "\n%d\n" (loop 0 0 (heighten_chamber 4 [||]) 0 2 0)
