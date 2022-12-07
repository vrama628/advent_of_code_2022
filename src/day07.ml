open Core

type pwd = string list
(** pwd ordering is from child to parent *)

type 'a fs =
  | File of int
  | Dir of (string, 'a fs, 'a) Map.t

(* (size of fs, size of dirs < 100000) *)
let rec summarize_help : 'a fs -> int * int = function
  | File size -> size, 0
  | Dir dir ->
    let total, summary =
      Map.fold dir ~init:(0, 0) ~f:(fun ~key:_ ~data (total_acc, summary_acc) ->
          let total, summary = summarize_help data in
          total_acc + total, summary_acc + summary
      )
    in
    let summary =
      if total < 100000 then
        summary + total
      else
        summary
    in
    total, summary

let summarize : 'a fs -> int = Fn.compose snd summarize_help

let rec add_to_pwd_help
    ~(rev_pwd : string list)
    ~(name : string)
    ~(to_add : 'a fs)
    (fs : 'a fs) : 'a fs =
  let dir = match fs with Dir dir -> dir | File _ -> failwith "unreachable" in
  match rev_pwd with
  | [] -> Dir (Map.add_exn dir ~key:name ~data:to_add)
  | dir_name :: rev_pwd ->
    Dir
      (Map.update
         dir
         dir_name
         ~f:
           (Fn.compose (add_to_pwd_help ~rev_pwd ~name ~to_add) Option.value_exn)
      )

let add_to_pwd ~(pwd : pwd) ~(fs : 'a fs) ~(name : string) ~(to_add : 'a fs) :
    'a fs =
  add_to_pwd_help ~rev_pwd:(List.rev pwd) ~name ~to_add fs

let empty_dir = Dir (Map.empty (module String))

let rec loop ~(pwd : pwd) ~(fs : 'a fs) : int =
  match In_channel.input_line In_channel.stdin with
  | None -> summarize fs
  | Some line -> (
    match line.[0] with
    | '$' -> (
      match line.[2] with
      | 'c' ->
        let pwd =
          match String.drop_prefix line 5 with
          | "/" -> []
          | ".." -> List.tl_exn pwd
          | dir_name -> dir_name :: pwd
        in
        loop ~pwd ~fs
      | _ ->
        (* ls; we know to consume ls output when we see it so just continue *)
        loop ~pwd ~fs
    )
    | 'd' ->
      let fs =
        add_to_pwd ~pwd ~fs ~name:(String.drop_prefix line 4) ~to_add:empty_dir
      in
      loop ~pwd ~fs
    | _ ->
      (* file *)
      let size, name = String.lsplit2_exn line ~on:' ' in
      let to_add = File (int_of_string size) in
      let fs = add_to_pwd ~pwd ~fs ~name ~to_add in
      loop ~pwd ~fs
  )

let () = Printf.printf "%d\n" (loop ~pwd:[] ~fs:empty_dir)
