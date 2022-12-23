open Core

type op =
  | Mul
  | Div
  | Add
  | Sub

let op_of_string = function
  | "*" -> Mul
  | "/" -> Div
  | "+" -> Add
  | "-" -> Sub
  | _ -> failwith "unreachable"

let eval_op = function
  | Mul -> ( * )
  | Div -> ( / )
  | Add -> ( + )
  | Sub -> ( - )

type monkey =
  | Const of int
  | Op of {
      op : op;
      lhs : string;
      rhs : string;
    }

type eval =
  | Resolved of int
  | Humn
  | Unresolved of {
      op : op;
      lhs : eval;
      rhs : eval;
    }

let rec unify n = function
  | Resolved _
  | Unresolved { lhs = Humn | Unresolved _; rhs = Humn | Unresolved _; _ } ->
    failwith "unreachable"
  | Humn -> n
  | Unresolved { lhs = Resolved operand; op = Mul; rhs = ast }
  | Unresolved { lhs = ast; op = Mul; rhs = Resolved operand } ->
    unify (n / operand) ast
  | Unresolved { lhs = Resolved operand; op = Add; rhs = ast }
  | Unresolved { lhs = ast; op = Add; rhs = Resolved operand } ->
    unify (n - operand) ast
  | Unresolved { lhs; op = Sub; rhs = Resolved operand } ->
    unify (n + operand) lhs
  | Unresolved { lhs = Resolved operand; op = Sub; rhs } ->
    unify (operand - n) rhs
  | Unresolved { lhs; op = Div; rhs = Resolved operand } ->
    unify (n * operand) lhs
  | Unresolved { lhs = Resolved operand; op = Div; rhs } ->
    unify (operand / n) rhs

let () =
  let monkeys =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:(fun line ->
           let key = String.prefix line 4 in
           let monkey =
             match String.drop_prefix line 6 |> String.split ~on:' ' with
             | [const] -> Const (int_of_string const)
             | [lhs; op_str; rhs] ->
               let op = op_of_string op_str in
               Op { lhs; op; rhs }
             | _ -> failwith "unreachable"
           in
           key, monkey
       )
    |> Map.of_alist_exn (module String)
  in
  let left, right =
    match Map.find_exn monkeys "root" with
    | Op { lhs; rhs; op = _ } -> lhs, rhs
    | Const _ -> failwith "unreachable"
  in
  let eval eval key =
    if String.equal key "humn" then
      Humn
    else
      match Map.find_exn monkeys key with
      | Const x -> Resolved x
      | Op { lhs; op; rhs } -> (
        match eval lhs, eval rhs with
        | Resolved lhs, Resolved rhs -> Resolved (eval_op op lhs rhs)
        | lhs, rhs -> Unresolved { lhs; op; rhs }
      )
  in
  let eval = Memo.recursive ~hashable:String.hashable eval in
  match eval left, eval right with
  | Resolved n, ast | ast, Resolved n -> printf "%d\n" (unify n ast)
  | _ -> failwith "unreachable"
