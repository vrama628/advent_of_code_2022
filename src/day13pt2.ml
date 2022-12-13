open Core

let rec compare (left : Yojson.Safe.t) (right : Yojson.Safe.t) =
  match left, right with
  | `Int l, `Int r -> Int.compare l r
  | `List l, `List r -> List.compare compare l r
  | `Int _, `List _ -> compare (`List [left]) right
  | `List _, `Int _ -> compare left (`List [right])
  | _ -> failwith "unreachable"

let () =
  let divider_1 = Yojson.Safe.from_string "[[2]]" in
  let divider_2 = Yojson.Safe.from_string "[[6]]" in
  let sorted_packets =
    In_channel.input_lines In_channel.stdin
    |> List.filter ~f:(Fn.compose not String.is_empty)
    |> List.map ~f:Yojson.Safe.from_string
    |> List.cons divider_1
    |> List.cons divider_2
    |> List.sort ~compare
  in
  let idx_1, _ =
    List.findi_exn sorted_packets ~f:(Fn.const (Yojson.Safe.equal divider_1))
  in
  let idx_2, _ =
    List.findi_exn sorted_packets ~f:(Fn.const (Yojson.Safe.equal divider_2))
  in
  printf "%d\n" ((idx_1 + 1) * (idx_2 + 1))
