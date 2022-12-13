open Core

let rec compare (left : Yojson.Safe.t) (right : Yojson.Safe.t) =
  match left, right with
  | `Int l, `Int r -> Int.compare l r
  | `List l, `List r -> List.compare compare l r
  | `Int _, `List _ -> compare (`List [left]) right
  | `List _, `Int _ -> compare left (`List [right])
  | _ -> failwith "unreachable"

let () =
  In_channel.input_lines In_channel.stdin
  |> List.chunks_of ~length:3
  |> List.map ~f:(function
         | left :: right :: _ ->
           Yojson.Safe.from_string left, Yojson.Safe.from_string right
         | _ -> failwith "unreachable"
         )
  |> List.filter_mapi ~f:(fun i (left, right) ->
         Option.some_if (compare left right < 0) (i + 1)
     )
  |> List.fold ~init:0 ~f:( + )
  |> printf "%d\n"
