open Base
open Stdio

let file = "day4.txt"

let read_file filename =
  let read_line ic =
    let rec read_line_aux ic acc =
      match In_channel.input_line ic with
      | Some line -> read_line_aux ic (line :: acc)
      | None -> acc
    in
    read_line_aux ic []
  in
  List.rev (In_channel.with_file filename ~f:read_line)

let data = read_file file
let () = List.iter ~f:(printf "%s\n") data

(* Represent interval [a,b]*)
type interval = { a : int; b : int }

let is_included i1 i2 =
  (i1.a >= i2.a && i1.a <= i2.b) && i1.b >= i2.a && i1.b <= i2.b

let is_overlap i1 i2 = not (i1.a > i2.b || i1.b < i2.a)

let line_to_intervals line =
  let str_to_interval str =
    match String.rsplit2 str ~on:'-' with
    | None -> failwith "Invalid input"
    | Some (a, b) -> { a = Int.of_string a; b = Int.of_string b }
  in
  match String.rsplit2 line ~on:',' with
  | None -> failwith "Invalid input"
  | Some (i1, i2) -> (str_to_interval i1, str_to_interval i2)

let result =
  let fully_contain = function
    | i1, i2 -> is_included i1 i2 || is_included i2 i1
  in
  let data' = List.map ~f:line_to_intervals data in
  let included = List.map ~f:fully_contain data' in
  let results = List.filter included ~f:(fun x -> x) in
  List.length results

let () = printf "Part 1 - %d\n" result

let result =
  let data' = List.map ~f:line_to_intervals data in
  let overlap = function i1, i2 -> is_overlap i1 i2 in
  let overlapings = List.map ~f:overlap data' in
  let results = List.filter overlapings ~f:(fun x -> x) in
  List.length results

let () = printf "Part 2 - %d\n" result
