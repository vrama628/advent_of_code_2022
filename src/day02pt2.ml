open Core

type choice =
  | Rock
  | Paper
  | Scissors

let choice_of_char = function
  | 'A' -> Rock
  | 'B' -> Paper
  | 'C' -> Scissors
  | _ -> failwith "unreachable"

type outcome =
  | Lose
  | Draw
  | Win

let outcome_of_char = function
  | 'X' -> Lose
  | 'Y' -> Draw
  | 'Z' -> Win
  | _ -> failwith "unreachable"

let shape_score = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let outcome_score = function Win -> 6 | Draw -> 3 | Lose -> 0

let choice_of_outcome ~opp ~outcome =
  match opp, outcome with
  | Scissors, Win | Rock, Draw | Paper, Lose -> Rock
  | Rock, Win | Paper, Draw | Scissors, Lose -> Paper
  | Paper, Win | Scissors, Draw | Rock, Lose -> Scissors

let rec loop acc =
  match In_channel.input_line In_channel.stdin with
  | None -> acc
  | Some line ->
    let opp, outcome = choice_of_char line.[0], outcome_of_char line.[2] in
    let you = choice_of_outcome ~opp ~outcome in
    loop (acc + shape_score you + outcome_score outcome)

let () = Printf.printf "%d\n" (loop 0)