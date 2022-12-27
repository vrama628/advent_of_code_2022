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

let intersects rock chamber r c =
  c < 0
  || c + width_of_rock rock > 7
  || r + height_of_rock rock > Array.length chamber
  || Array.existsi rock ~f:(fun row_i row ->
         Array.existsi row ~f:(fun col_i elt ->
             elt && chamber.(r + row_i).(c + col_i)
         )
     )

module Key = struct
  type t = int * bool list list [@@deriving ord, sexp_of, hash]
end

type memo_entry = {
  rock : int;
  height : int;
}

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
  let memo_table = Hashtbl.create (module Key) in
  let rec loop rock jet chamber r c =
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
      let highest_nonempty_row = highest_nonempty_row_of_chamber chamber in
      let key =
        ( rock mod 5,
          chamber
          |> Array.sub
               ~pos:highest_nonempty_row
               ~len:(min 20 (Array.length chamber - highest_nonempty_row))
          |> Array.to_list
          |> List.map ~f:Array.to_list )
      in
      match Hashtbl.find memo_table key with
      | Some { rock = iso_rock; height } ->
        let rock_stride = rock - iso_rock in
        let row_stride = Array.length chamber - highest_nonempty_row - height in
        let strides = (1000000000000 - iso_rock) / rock_stride in
        let last_rock_offset = (1000000000000 - iso_rock) % rock_stride in
        let iso_last_rock = last_rock_offset + iso_rock in
        let iso_res =
          With_return.with_return (fun { return } ->
              Hashtbl.fold
                memo_table
                ~init:()
                ~f:(fun ~key:_ ~data:{ rock; height } () ->
                  if rock = iso_last_rock then return height
              );
              failwith "unreachable"
          )
        in
        iso_res + (strides * row_stride) - 1
      | None ->
        Hashtbl.add_exn
          memo_table
          ~key
          ~data:{ rock; height = Array.length chamber - highest_nonempty_row };
        let row =
          highest_nonempty_row - 3 - height_of_rock rocks.((rock + 1) mod 5)
        in
        let heighten_chamber_by = max 0 (-row) in
        let chamber = heighten_chamber heighten_chamber_by chamber in
        loop
          (rock + 1)
          ((jet + 1) mod Array.length jets)
          chamber
          (row + heighten_chamber_by)
          2
    ) else
      loop rock ((jet + 1) mod Array.length jets) chamber (r + 1) c
  in
  printf "%d\n" (loop 0 0 (heighten_chamber 4 [||]) 0 2)
