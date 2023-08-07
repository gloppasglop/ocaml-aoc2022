open Base
open Stdio

let file = "day2.txt"
let data = In_channel.read_lines file
let string_to_move s = (String.get s 0, String.get s 2)
let moves = List.map data ~f:string_to_move

let playscore move =
  match move with
  | 'A', 'X' -> 1 + 3
  | 'A', 'Y' -> 2 + 6
  | 'A', 'Z' -> 3 + 0
  | 'B', 'X' -> 1 + 0
  | 'B', 'Y' -> 2 + 3
  | 'B', 'Z' -> 3 + 6
  | 'C', 'X' -> 1 + 6
  | 'C', 'Y' -> 2 + 0
  | 'C', 'Z' -> 3 + 3
  | _ -> failwith "Invalid move"

let scores = List.map moves ~f:playscore
let () = printf "Part 1 - %d\n" (List.fold_left scores ~f:( + ) ~init:0)

let playscore = function
  | 'A', 'X' -> 3 + 0
  | 'A', 'Y' -> 1 + 3
  | 'A', 'Z' -> 2 + 6
  | 'B', 'X' -> 1 + 0
  | 'B', 'Y' -> 2 + 3
  | 'B', 'Z' -> 3 + 6
  | 'C', 'X' -> 2 + 0
  | 'C', 'Y' -> 3 + 3
  | 'C', 'Z' -> 1 + 6
  | _ -> failwith "Invalid move"

let scores = List.map moves ~f:playscore
let () = printf "Part 2 - %d\n" (List.fold_left scores ~init:0 ~f:( + ))
