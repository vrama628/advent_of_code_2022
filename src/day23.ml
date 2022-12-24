open Core

module TupleCmp = Tuple.Comparator (Int) (Int)

type coords = int * int

type consideration = {
  considerations : coords list;
  proposal : coords;
}

let rotate_considerations = function
  | x :: xs -> xs @ [x]
  | [] -> failwith "unreachable"

let ( ++ ) (i1, j1) (i2, j2) = i1 + i2, j1 + j2

let adjacents coords =
  [
    coords ++ (-1, -1);
    coords ++ (-1, 0);
    coords ++ (-1, 1);
    coords ++ (0, 1);
    coords ++ (1, 1);
    coords ++ (1, 0);
    coords ++ (1, -1);
    coords ++ (0, -1);
  ]

let do_round (elves, considerations) =
  let proposals =
    Set.to_list elves
    |> List.map ~f:(fun coords ->
           if List.for_all (adjacents coords) ~f:(Fn.non (Set.mem elves)) then
             coords, coords
           else
             match
               List.find considerations ~f:(fun { considerations; _ } ->
                   List.for_all considerations ~f:(fun consideration ->
                       not (Set.mem elves (coords ++ consideration))
                   )
               )
             with
             | None -> coords, coords
             | Some { proposal; _ } -> coords ++ proposal, coords
       )
    |> Map.of_alist_multi (module TupleCmp)
  in
  let uncontested, constested =
    Map.partition_tf proposals ~f:(fun elves -> List.length elves = 1)
  in
  let elves =
    List.fold
      (Map.data constested |> List.concat)
      ~init:(Map.key_set uncontested)
      ~f:Set.add
  in
  elves, rotate_considerations considerations

let () =
  let elves =
    In_channel.input_lines In_channel.stdin
    |> List.foldi
         ~init:(Set.empty (module TupleCmp))
         ~f:(fun i init ->
           String.foldi ~init ~f:(fun j acc c ->
               if Char.(c = '#') then
                 Set.add acc (i, j)
               else
                 acc
           )
         )
  in
  let considerations =
    [
      { proposal = -1, 0; considerations = [-1, -1; -1, 0; -1, 1] };
      { proposal = 1, 0; considerations = [1, -1; 1, 0; 1, 1] };
      { proposal = 0, -1; considerations = [-1, -1; 0, -1; 1, -1] };
      { proposal = 0, 1; considerations = [-1, 1; 0, 1; 1, 1] };
    ]
  in
  let elves, _ = Fn.apply_n_times ~n:10 do_round (elves, considerations) in
  let rectangle_size =
    let min_i, min_j, max_i, max_j =
      Set.fold
        elves
        ~init:(Int.max_value, Int.max_value, Int.min_value, Int.min_value)
        ~f:(fun (min_i, min_j, max_i, max_j) (i, j) ->
          min i min_i, min j min_j, max i max_i, max j max_j
      )
    in
    (max_i + 1 - min_i) * (max_j + 1 - min_j)
  in
  printf "%d\n" (rectangle_size - Set.length elves)