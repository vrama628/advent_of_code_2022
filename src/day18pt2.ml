open Core

module Cmp = struct
  type t = int * int * int
  include Comparator.Make (struct
    type t = int * int * int
    let compare =
      Tuple3.compare ~cmp1:Int.compare ~cmp2:Int.compare ~cmp3:Int.compare
    let sexp_of_t = Tuple3.sexp_of_t Int.sexp_of_t Int.sexp_of_t Int.sexp_of_t
  end)
end

let adjacent (x, y, z) =
  Set.of_list
    (module Cmp)
    [
      x + 1, y, z;
      x - 1, y, z;
      x, y + 1, z;
      x, y - 1, z;
      x, y, z + 1;
      x, y, z - 1;
    ]

let () =
  let lava =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:(fun line ->
           line |> String.split ~on:',' |> List.map ~f:int_of_string |> function
           | [x; y; z] -> x, y, z
           | _ -> failwith "unreachable"
       )
    |> Set.of_list (module Cmp)
  in
  let min_pt, max_pt =
    Set.fold
      lava
      ~init:
        ( (Int.max_value, Int.max_value, Int.max_value),
          (Int.min_value, Int.min_value, Int.min_value)
        )
      ~f:(fun (min_pt, max_pt) pt ->
        Tuple3.map2 ~f:min min_pt pt, Tuple3.map2 ~f:max max_pt pt
      )
    |> Tuple2.map_fst ~f:(Tuple3.map ~f:(( + ) (-1)))
    |> Tuple2.map_snd ~f:(Tuple3.map ~f:(( + ) 2))
  in
  let rec flow_air ~air ~frontier =
    match Set.choose frontier with
    | None -> air
    | Some pt ->
      let frontier = Set.remove frontier pt in
      let air = Set.add air pt in
      let frontier =
        Set.union
          frontier
          (Set.diff (Set.diff (adjacent pt) air) lava
          |> Set.filter ~f:(fun pt ->
                 match
                   ( Tuple3.map2 ~f:( <= ) min_pt pt,
                     Tuple3.map2 ~f:( < ) pt max_pt )
                 with
                 | (true, true, true), (true, true, true) -> true
                 | _ -> false
             )
          )
      in
      flow_air ~air ~frontier
  in
  let air =
    flow_air
      ~air:(Set.empty (module Cmp))
      ~frontier:(Set.singleton (module Cmp) min_pt)
  in
  let air_surface_area =
    Set.sum
      (module Int)
      air
      ~f:(fun pt -> Set.length (Set.diff (adjacent pt) air))
  in
  let outer_surface_area =
    let dx, dy, dz = Tuple3.map2 max_pt min_pt ~f:( - ) in
    2 * ((dx * dy) + (dx * dz) + (dy * dz))
  in
  printf "%d\n" (air_surface_area - outer_surface_area)
