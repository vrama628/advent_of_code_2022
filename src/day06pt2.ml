open Core

let () =
  let buffer = In_channel.input_line_exn In_channel.stdin in
  Printf.printf
    "%d\n"
    (With_return.with_return (fun { return } ->
         ignore
           (String.foldi buffer ~init:[] ~f:(fun i acc c ->
                let acc = List.take (c :: acc) 14 in
                if Set.length (Set.of_list (module Char) acc) = 14 then
                  return (i + 1);
                acc
            )
           );
         failwith "unreachable"
     )
    )
