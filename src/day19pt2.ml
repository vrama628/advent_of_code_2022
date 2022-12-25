open Core

module Cost = struct
  type t = {
    ore : int;
    clay : int;
    obsidian : int;
  }
  [@@deriving ord, sexp_of, hash]

  let make ?(ore = 0) ?(clay = 0) ?(obsidian = 0) () = { ore; clay; obsidian }

  let ( + )
      { ore = ore1; clay = clay1; obsidian = obsidian1 }
      { ore = ore2; clay = clay2; obsidian = obsidian2 } =
    {
      ore = ore1 + ore2;
      clay = clay1 + clay2;
      obsidian = obsidian1 + obsidian2;
    }

  let ( - )
      { ore = ore1; clay = clay1; obsidian = obsidian1 }
      { ore = ore2; clay = clay2; obsidian = obsidian2 } =
    {
      ore = ore1 - ore2;
      clay = clay1 - clay2;
      obsidian = obsidian1 - obsidian2;
    }

  let is_natural { ore; clay; obsidian } =
    ore >= 0 && clay >= 0 && obsidian >= 0
end

module Blueprint = struct
  type t = {
    ore : Cost.t;
    clay : Cost.t;
    obsidian : Cost.t;
    geode : Cost.t;
  }
end

module MemoKey = struct
  type t = {
    resources : Cost.t;
    time : int;
    ore_bots : int;
    clay_bots : int;
    obsidian_bots : int;
  }
  [@@deriving ord, sexp_of, hash]
end

let evaluate blueprint =
  printf "\nevaluating blueprint...\n%!";
  let memo_table = Hashtbl.create (module MemoKey) in
  let rec eval ~resources ~time ~ore_bots ~clay_bots ~obsidian_bots =
    if Float.(Random.float 1. < 0.00001) then
      printf "\r%12d%!" (Hashtbl.length memo_table);
    Hashtbl.find_or_add
      memo_table
      MemoKey.{ resources; time; ore_bots; clay_bots; obsidian_bots }
      ~default:(fun () ->
        if time >= 32 then
          0
        else
          let time = time + 1 in
          let new_resources =
            Cost.make ~ore:ore_bots ~clay:clay_bots ~obsidian:obsidian_bots ()
          in
          [
            Some
              (eval
                 ~resources:Cost.(resources + new_resources)
                 ~time
                 ~ore_bots
                 ~clay_bots
                 ~obsidian_bots
              );
            (let resources = Cost.(resources - blueprint.Blueprint.ore) in
             if Cost.is_natural resources then
               Some
                 (eval
                    ~resources:Cost.(resources + new_resources)
                    ~time
                    ~ore_bots:(ore_bots + 1)
                    ~clay_bots
                    ~obsidian_bots
                 )
             else
               None
            );
            (let resources = Cost.(resources - blueprint.Blueprint.clay) in
             if Cost.is_natural resources then
               Some
                 (eval
                    ~resources:Cost.(resources + new_resources)
                    ~time
                    ~ore_bots
                    ~clay_bots:(clay_bots + 1)
                    ~obsidian_bots
                 )
             else
               None
            );
            (let resources = Cost.(resources - blueprint.Blueprint.obsidian) in
             if Cost.is_natural resources then
               Some
                 (eval
                    ~resources:Cost.(resources + new_resources)
                    ~time
                    ~ore_bots
                    ~clay_bots
                    ~obsidian_bots:(obsidian_bots + 1)
                 )
             else
               None
            );
            (let resources = Cost.(resources - blueprint.Blueprint.geode) in
             if Cost.is_natural resources then
               Some
                 (32
                 - time
                 + eval
                     ~resources:Cost.(resources + new_resources)
                     ~time
                     ~ore_bots
                     ~clay_bots
                     ~obsidian_bots
                 )
             else
               None
            );
          ]
          |> List.filter_opt
          |> List.max_elt ~compare
          |> Option.value_exn
      )
  in
  eval
    ~resources:(Cost.make ())
    ~time:0
    ~ore_bots:1
    ~clay_bots:0
    ~obsidian_bots:0

let () =
  In_channel.input_lines In_channel.stdin
  |> Fn.flip List.take 3
  |> List.map ~f:(fun line ->
         Scanf.sscanf
           line
           "Blueprint %_d: Each ore robot costs %d ore. Each clay robot costs \
            %d ore. Each obsidian robot costs %d ore and %d clay. Each geode \
            robot costs %d ore and %d obsidian."
           (fun
             ore_ore
             clay_ore
             obsidian_ore
             obsidian_clay
             geode_ore
             geode_obsidian
           ->
             Blueprint.
               {
                 ore = Cost.make ~ore:ore_ore ();
                 clay = Cost.make ~ore:clay_ore ();
                 obsidian = Cost.make ~ore:obsidian_ore ~clay:obsidian_clay ();
                 geode = Cost.make ~ore:geode_ore ~obsidian:geode_obsidian ();
               }
         )
     )
  |> List.map ~f:evaluate
  |> List.fold ~init:1 ~f:( * )
  |> printf "\n%d\n"
