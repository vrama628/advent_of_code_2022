open Core

let summarize (grid : char array array) : int =
  Array.foldi grid ~init:0 ~f:(fun i acc row ->
      acc
      + Array.counti row ~f:(fun j tree ->
            Array.for_alli grid ~f:(fun i' row' ->
                i' >= i || Char.(tree > row'.(j))
            )
            || Array.for_alli grid ~f:(fun i' row' ->
                   i' <= i || Char.(tree > row'.(j))
               )
            || Array.for_alli row ~f:(fun j' tree' ->
                   j' >= j || Char.(tree > tree')
               )
            || Array.for_alli row ~f:(fun j' tree' ->
                   j' <= j || Char.(tree > tree')
               )
        )
  )

let rec loop (acc : char array list) : int =
  match In_channel.input_line In_channel.stdin with
  | None -> summarize (Array.of_list acc)
  | Some line -> loop (String.to_array line :: acc)

let () = Printf.printf "%d\n" (loop [])
