open Core

let () =
  let heightmap =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:String.to_array
    |> Array.of_list
  in
  let s_i, s_j =
    Array.find_mapi_exn heightmap ~f:(fun i row ->
        Array.find_mapi row ~f:(fun j c -> Option.some_if Char.(c = 'S') (i, j))
    )
  in
  heightmap.(s_i).(s_j) <- 'a';
  let start =
    Array.find_mapi_exn heightmap ~f:(fun i row ->
        Array.find_mapi row ~f:(fun j c -> Option.some_if Char.(c = 'E') (i, j))
    )
  in
  heightmap.(fst start).(snd start) <- 'z';
  let queue = Queue.create () in
  let seen = Hash_set.create (module Tuple.Hashable_t (Int) (Int)) in
  Queue.enqueue queue (start, 0);
  Hash_set.add seen start;
  let rec bfs () =
    let (ui, uj), dist = Queue.dequeue_exn queue in
    if Char.(heightmap.(ui).(uj) = 'a') then
      dist
    else (
      [ui + 1, uj; ui - 1, uj; ui, uj + 1; ui, uj - 1]
      |> List.filter ~f:(fun (i, j) ->
             i >= 0
             && i < Array.length heightmap
             && j >= 0
             && j < Array.length heightmap.(0)
             && Char.to_int heightmap.(ui).(uj) - Char.to_int heightmap.(i).(j)
                <= 1
             && not (Hash_set.mem seen (i, j))
         )
      |> List.iter ~f:(fun v ->
             Hash_set.add seen v;
             Queue.enqueue queue (v, dist + 1)
         );
      bfs ()
    )
  in
  printf "%d\n" (bfs ())