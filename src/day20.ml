open Core

type mix = {
  position : int;
  value : int;
}

let () =
  let encrypted =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:int_of_string
    |> Array.of_list_mapi ~f:(fun position value -> { position; value })
  in
  for i = 0 to Array.length encrypted - 1 do
    let old_position = encrypted.(i).position in
    let new_position =
      (old_position + encrypted.(i).value) % (Array.length encrypted - 1)
    in
    if old_position < new_position then
      Array.map_inplace encrypted ~f:(fun mix ->
          if old_position < mix.position && mix.position <= new_position then
            { mix with position = mix.position - 1 }
          else
            mix
      )
    else
      Array.map_inplace encrypted ~f:(fun mix ->
          if new_position <= mix.position && mix.position < old_position then
            { mix with position = mix.position + 1 }
          else
            mix
      );
    encrypted.(i) <- { (encrypted.(i)) with position = new_position }
  done;
  let decrypted = Array.create ~len:(Array.length encrypted) 0 in
  Array.iter encrypted ~f:(fun { position; value } ->
      decrypted.(position) <- value
  );
  let zero_idx = Array.findi_exn decrypted ~f:(Fn.const (( = ) 0)) |> fst in
  printf
    "%d\n"
    (decrypted.((zero_idx + 1000) % Array.length decrypted)
    + decrypted.((zero_idx + 2000) % Array.length decrypted)
    + decrypted.((zero_idx + 3000) % Array.length decrypted)
    )
