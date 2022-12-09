open Core

let move ~t:(ti, tj) ~h:(hi, hj) : int * int =
  if abs (ti - hi) <= 1 && abs (tj - hj) <= 1 then
    ti, tj
  else
    ti + compare hi ti, tj + compare hj tj

let ( ++ ) (i1, j1) (i2, j2) = i1 + i2, j1 + j2

let rec loop
    ~(positions : (int * int, 'a) Set.t)
    ~(head : int * int)
    ~(tail : (int * int) list) : int =
  match In_channel.input_line In_channel.stdin with
  | None -> Set.length positions
  | Some line ->
    let direction, steps = String.lsplit2_exn line ~on:' ' in
    let delta =
      match direction with
      | "U" -> -1, 0
      | "D" -> 1, 0
      | "L" -> 0, -1
      | "R" -> 0, 1
      | _ -> failwith "unreachable"
    in
    let n = int_of_string steps in
    let positions, head, tail =
      Fn.apply_n_times
        ~n
        (fun (positions, head, tail) ->
          let head = head ++ delta in
          let t, tail =
            List.fold_map tail ~init:head ~f:(fun h t ->
                let t = move ~t ~h in
                t, t
            )
          in
          let positions = Set.add positions t in
          positions, head, tail
        )
        (positions, head, tail)
    in
    loop ~positions ~head ~tail

let () =
  Printf.printf
    "%d\n"
    (loop
       ~positions:
         (Set.Using_comparator.empty
            ~comparator:(Tuple2.comparator Int.comparator Int.comparator)
         )
       ~head:(0, 0)
       ~tail:(List.init 9 ~f:(Fn.const (0, 0)))
    )
