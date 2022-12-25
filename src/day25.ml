open Core

let snafu_of_string str =
  str
  |> String.to_list
  |> List.rev
  |> List.mapi ~f:(fun i c ->
         let digit =
           match c with
           | '2' -> 2
           | '1' -> 1
           | '0' -> 0
           | '-' -> -1
           | '=' -> -2
           | _ -> failwith "unreachable"
         in
         digit * Int.(5 ** i)
     )
  |> List.sum (module Int) ~f:Fn.id

let rec string_of_snafu n =
  if n = 0 then
    ""
  else
    let c, d =
      match n % 5 with
      | 4 -> "-", -1
      | 3 -> "=", -2
      | 0 -> "0", 0
      | 1 -> "1", 1
      | 2 -> "2", 2
      | _ -> failwith "unreachable"
    in
    string_of_snafu ((n - d) / 5) ^ c

let () =
  let sum =
    In_channel.input_lines In_channel.stdin
    |> List.sum (module Int) ~f:snafu_of_string
  in
  printf "%s\n" (string_of_snafu sum)