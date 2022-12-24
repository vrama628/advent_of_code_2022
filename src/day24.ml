open Core

type direction =
  | Up
  | Down
  | Left
  | Right
[@@deriving ord, sexp_of, enumerate]

let direction_of_char = function
  | '^' -> Up
  | 'v' -> Down
  | '<' -> Left
  | '>' -> Right
  | _ -> failwith "invalid direction"

let move direction (i, j) =
  match direction with
  | Up -> i - 1, j
  | Down -> i + 1, j
  | Left -> i, j - 1
  | Right -> i, j + 1

module Cmp =
  Tuple.Comparator
    (Tuple.Comparator (Int) (Int))
       (struct
         type t = direction
         include Comparator.Make (struct
           type t = direction
           let compare = compare_direction
           let sexp_of_t = sexp_of_direction
         end)
       end)

module HashKey = struct
  type t = (int * int) * int [@@deriving sexp_of]
  let compare = Stdlib.compare
  let hash = Hashtbl.hash
end

let () =
  let valley, min_i, min_j, max_i, max_j =
    In_channel.input_lines In_channel.stdin
    |> List.foldi
         ~init:
           ( Set.empty (module Cmp),
             Int.max_value,
             Int.max_value,
             Int.min_value,
             Int.min_value
           )
         ~f:(fun i init ->
           String.foldi ~init ~f:(fun j (valley, min_i, min_j, max_i, max_j) ->
             function
             | '.' -> valley, min i min_i, min j min_j, max i max_i, max j max_j
             | '#' -> valley, min_i, min_j, max_i, max_j
             | c ->
               ( Set.add valley ((i, j), direction_of_char c),
                 min_i,
                 min_j,
                 max_i,
                 max_j )
           )
         )
  in
  let start = min_i, min_j in
  let end_ = max_i, max_j in
  let min_i, max_i = min_i + 1, max_i - 1 in
  let mod_blizzard (i, j) =
    ( ((i - min_i) % (max_i - min_i + 1)) + min_i,
      ((j - min_j) % (max_j - min_j + 1)) + min_j )
  in
  let n_unique_valleys =
    Z.lcm (Z.of_int (max_i - min_i + 1)) (Z.of_int (max_j - min_j + 1))
    |> Z.to_int
  in
  let in_bounds pos =
    Poly.(pos = start)
    || Poly.(pos = end_)
    || Int.between (fst pos) ~low:min_i ~high:max_i
       && Int.between (snd pos) ~low:min_j ~high:max_j
  in
  let step_valley =
    Set.map
      (module Cmp)
      ~f:(fun (pos, direction) -> mod_blizzard (move direction pos), direction)
  in
  let queue = Queue.singleton (start, valley, 0) in
  let hash_key_of ~pos ~time = pos, time % n_unique_valleys in
  let seen = Hash_set.create (module HashKey) in
  Hash_set.add seen (hash_key_of ~pos:start ~time:0);
  let rec bfs () =
    let pos, valley, time = Queue.dequeue_exn queue in
    Hash_set.add seen (hash_key_of ~pos ~time);
    if Poly.(pos = end_) then
      time
    else
      let valley = step_valley valley in
      all_of_direction
      |> List.map ~f:(fun direction -> move direction pos)
      |> List.cons pos
      |> List.filter ~f:(fun pos ->
             in_bounds pos
             && (not
                   (Set.exists
                      valley
                      ~f:Poly.(fun (blizzard, _) -> blizzard = pos)
                   )
                )
             && not (Hash_set.mem seen (hash_key_of ~pos ~time:(time + 1)))
         )
      |> List.map ~f:(fun pos ->
             Hash_set.add seen (hash_key_of ~pos ~time:(time + 1));
             pos, valley, time + 1
         )
      |> Queue.enqueue_all queue;
      bfs ()
  in
  printf "\n%d\n" (bfs ())
