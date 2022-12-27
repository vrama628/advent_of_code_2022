open Core

type valve = {
  tunnels : string list;
  rate : int;
}

module MemoKey = struct
  type t = {
    human_valve : string;
    phant_valve : string;
    time : int;
    opened : string list;
  }
  [@@deriving ord, sexp_of, hash]
end

let () =
  let valves =
    In_channel.input_lines In_channel.stdin
    |> List.fold
         ~init:(Map.empty (module String))
         ~f:(fun acc line ->
           Scanf.sscanf
             line
             "Valve %s has flow rate=%d; tunnel%_[s] lead%_[s] to valve%_[s] \
              %[A-Z, ]%!"
             (fun key rate tunnels ->
               let tunnels =
                 List.init
                   ((String.length tunnels / 4) + 1)
                   ~f:(fun i ->
                     String.of_char_list
                       [tunnels.[4 * i]; tunnels.[(4 * i) + 1]]
                   )
               in
               Map.add_exn acc ~key ~data:{ tunnels; rate }
           )
         )
  in
  let num_valves = Map.length valves in
  let memo_table = Hashtbl.create (module MemoKey) in
  let rec travel human_valve phant_valve time opened report =
    Hashtbl.find_or_add
      memo_table
      MemoKey.{ human_valve; phant_valve; time; opened = Set.to_list opened }
      ~default:(fun () ->
        if time >= 26 || Set.length opened = num_valves then
          0
        else
          let human_moves =
            (Map.find_exn valves human_valve).tunnels
            |> List.map ~f:(fun tunnel -> tunnel, [])
          in
          let phant_moves =
            (Map.find_exn valves phant_valve).tunnels
            |> List.map ~f:(fun tunnel -> tunnel, [])
          in
          let human_choices =
            if
              Set.mem opened human_valve
              || (Map.find_exn valves human_valve).rate = 0
            then
              human_moves
            else
              (human_valve, [human_valve]) :: human_moves
          in
          let phant_choices =
            if
              Set.mem opened phant_valve
              || (Map.find_exn valves phant_valve).rate = 0
            then
              phant_moves
            else
              (phant_valve, [phant_valve]) :: phant_moves
          in
          let all_choices =
            List.cartesian_product human_choices phant_choices
            |> List.permute
            |>
            if String.(human_valve = phant_valve) then
              List.filter
                ~f:(fun ((human_valve, human_opens), (phant_valve, phant_opens))
                   ->
                  String.(human_valve <= phant_valve)
                  && (List.is_empty human_opens || List.is_empty phant_opens)
              )
            else
              Fn.id
          in
          let num_choices = List.length all_choices in
          List.foldi
            all_choices
            ~init:0
            ~f:(fun
                 i
                 acc
                 ((human_valve, human_opens), (phant_valve, phant_opens))
               ->
              let opened, additional_flow =
                List.fold
                  (human_opens @ phant_opens)
                  ~init:(opened, 0)
                  ~f:(fun (opened, flow) valve ->
                    ( Set.add opened valve,
                      flow
                      + ((Map.find_exn valves valve).rate * (26 - (time + 1))) )
                )
              in
              let report f = report ((float i +. f) /. float num_choices) in
              report 0.;
              max
                acc
                (additional_flow
                + travel human_valve phant_valve (time + 1) opened report
                )
          )
      )
  in
  let start_time = Time.now () in
  let zone = Time.Zone.of_utc_offset ~hours:(-6) in
  let report f =
    if Float.(Random.float 1. < 0.00001) then
      let finish_time =
        try
          Time.add
            start_time
            Core_private.Span_float.(Time.diff (Time.now ()) start_time / f)
          |> Time.to_sec_string ~zone
        with Failure s -> s
      in
      printf
        "\r%.16f\t%20d\tExpected to finish at %s%!"
        f
        (Hashtbl.length memo_table)
        finish_time
  in
  printf "\n%d\n" (travel "AA" "AA" 0 (Set.empty (module String)) report)
