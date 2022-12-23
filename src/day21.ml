open Core

type monkey =
  | Const of int
  | Op of {
      op : int -> int -> int;
      lhs : string;
      rhs : string;
    }

let () =
  let monkeys =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:(fun line ->
           let key = String.prefix line 4 in
           let monkey =
             match String.drop_prefix line 6 |> String.split ~on:' ' with
             | [const] -> Const (int_of_string const)
             | [lhs; op_str; rhs] ->
               let op =
                 match op_str with
                 | "*" -> ( * )
                 | "/" -> ( / )
                 | "+" -> ( + )
                 | "-" -> ( - )
                 | _ -> failwith "unreachable"
               in
               Op { lhs; op; rhs }
             | _ -> failwith "unreachable"
           in
           key, monkey
       )
    |> Map.of_alist_exn (module String)
  in
  let eval eval key =
    match Map.find_exn monkeys key with
    | Const x -> x
    | Op { lhs; op; rhs } -> op (eval lhs) (eval rhs)
  in
  let eval = Memo.recursive ~hashable:String.hashable eval in
  printf "%d\n" (eval "root")
