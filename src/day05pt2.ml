open Core

type stacks = char list array

let tops stacks =
  stacks |> Array.to_list |> List.map ~f:List.hd_exn |> String.of_char_list

(* 0-indexed *)
let move ~n ~from ~to_ (stacks : stacks) : unit =
  stacks.(to_) <- List.take stacks.(from) n @ stacks.(to_);
  stacks.(from) <- List.drop stacks.(from) n

let rec loop (stacks : stacks) =
  match In_channel.input_line In_channel.stdin with
  | None -> tops stacks
  | Some line ->
    let n, from, to_ =
      match String.split line ~on:' ' with
      | [_; move; _; from; _; to_] ->
        int_of_string move, int_of_string from - 1, int_of_string to_ - 1
      | _ -> failwith "unreachable"
    in
    move ~n ~from ~to_ stacks;
    loop stacks

let rec read_stacks () : stacks =
  let line = In_channel.input_line_exn In_channel.stdin in
  if String.is_empty line then
    Array.init 9 ~f:(Fn.const [])
  else
    let stacks = read_stacks () in
    String.iteri line ~f:(fun i c ->
        if i % 4 = 1 && Char.(c <> ' ') then
          stacks.(i / 4) <- c :: stacks.(i / 4)
    );
    stacks

let () =
  let stacks = read_stacks () in
  Printf.printf "%s\n" (loop stacks)