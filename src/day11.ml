open Core

type monkey = {
  items : int Queue.t;
  operation : int -> int;
  test : int -> bool;
  if_true : int;
  if_false : int;
}

let rec parse_monkeys () : monkey list =
  let open In_channel in
  match input_line stdin with
  | None -> []
  | Some _ ->
    let monkey =
      let items =
        input_line_exn stdin
        |> String.chop_prefix_exn ~prefix:"  Starting items: "
        |> String.split ~on:','
        |> List.map ~f:(String.filter ~f:Char.is_digit)
        |> List.map ~f:int_of_string
        |> Queue.of_list
      in
      let operation =
        let expr =
          input_line_exn stdin
          |> String.chop_prefix_exn ~prefix:"  Operation: new = old "
        in
        if String.(expr = "* old") then
          fun item ->
        item * item
        else
          let op =
            match expr.[0] with
            | '*' -> ( * )
            | '+' -> ( + )
            | _ -> failwith "unreachable"
          in
          let n = String.drop_prefix expr 2 |> int_of_string in
          op n
      in
      let test =
        let divisor =
          input_line_exn stdin
          |> String.chop_prefix_exn ~prefix:"  Test: divisible by "
          |> int_of_string
        in
        fun item -> item % divisor = 0
      in
      let if_true =
        (input_line_exn stdin).[29] |> Char.to_string |> int_of_string
      in
      let if_false =
        (input_line_exn stdin).[30] |> Char.to_string |> int_of_string
      in
      ignore (input_line stdin);
      { items; operation; test; if_true; if_false }
    in
    monkey :: parse_monkeys ()

let () =
  let monkeys = parse_monkeys () |> Array.of_list in
  let activity = Array.map monkeys ~f:(Fn.const 0) in
  Fn.apply_n_times
    ~n:20
    (fun () ->
      Array.iteri
        monkeys
        ~f:(fun i { items; operation; test; if_true; if_false } ->
          activity.(i) <- activity.(i) + Queue.length items;
          Queue.iter items ~f:(fun item ->
              let item = operation item / 3 in
              Queue.enqueue
                monkeys.(if test item then
                           if_true
                         else
                           if_false)
                  .items
                item
          );
          Queue.clear items
      )
    )
    ();
  Array.sort activity ~compare:Int.compare;
  printf
    "%d\n"
    (activity.(Array.length activity - 1) * activity.(Array.length activity - 2))
