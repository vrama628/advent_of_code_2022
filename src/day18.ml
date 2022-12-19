open Core

module Cmp = struct
  type t = int * int * int
  include Comparator.Make (struct
    type t = int * int * int
    let compare =
      Tuple3.compare ~cmp1:Int.compare ~cmp2:Int.compare ~cmp3:Int.compare
    let sexp_of_t = Tuple3.sexp_of_t Int.sexp_of_t Int.sexp_of_t Int.sexp_of_t
  end)
end

let () =
  let cubes =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:(fun line ->
           line |> String.split ~on:',' |> List.map ~f:int_of_string |> function
           | [x; y; z] -> x, y, z
           | _ -> failwith "unreachable"
       )
    |> Set.of_list (module Cmp)
  in
  printf
    "%d\n"
    (Set.sum
       (module Int)
       cubes
       ~f:(fun (x, y, z) ->
         Set.length
           (Set.diff
              (Set.of_list
                 (module Cmp)
                 [
                   x + 1, y, z;
                   x - 1, y, z;
                   x, y + 1, z;
                   x, y - 1, z;
                   x, y, z + 1;
                   x, y, z - 1;
                 ]
              )
              cubes
           )
       )
    )
