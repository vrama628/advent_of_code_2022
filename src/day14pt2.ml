open Core

module Comparator = Tuple.Comparator (Int) (Int)
let empty = Set.empty (module Comparator)

let rec parse_input_line = function
  | (i1, j1) :: (i2, j2) :: rest ->
    let rocks =
      if i1 = i2 then
        List.range ~stride:(compare j2 j1) ~stop:`inclusive j1 j2
        |> List.map ~f:(fun j -> i1, j)
      else
        List.range ~stride:(compare i2 i1) ~stop:`inclusive i1 i2
        |> List.map ~f:(fun i -> i, j1)
    in
    List.fold rocks ~init:(parse_input_line ((i2, j2) :: rest)) ~f:Set.add
  | [_] | [] -> empty

let () =
  let rock =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:(fun line ->
           line
           |> String.split ~on:' '
           |> List.filter ~f:String.(( <> ) "->")
           |> List.map ~f:(String.lsplit2_exn ~on:',')
           |> List.map ~f:(Tuple.T2.map ~f:int_of_string)
           |> parse_input_line
       )
    |> Set.union_list (module Comparator)
  in
  let floor =
    Set.to_list rock
    |> List.map ~f:snd
    |> List.max_elt ~compare
    |> Option.value_exn
    |> ( + ) 2
  in
  let rec pour sand =
    let not_blocked point =
      snd point < floor && not (Set.mem rock point || Set.mem sand point)
    in
    let rec pour_unit (i, j) =
      if not_blocked (i, j + 1) then
        pour_unit (i, j + 1)
      else if not_blocked (i - 1, j + 1) then
        pour_unit (i - 1, j + 1)
      else if not_blocked (i + 1, j + 1) then
        pour_unit (i + 1, j + 1)
      else
        i, j
    in
    match pour_unit (500, 0) with
    | 500, 0 -> Set.length sand + 1
    | point -> pour (Set.add sand point)
  in
  printf "%d\n" (pour empty)