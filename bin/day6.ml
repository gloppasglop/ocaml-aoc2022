open Base
open Stdio

let file = "day6.txt"
let data = In_channel.read_lines file
let message = match data with head :: _ -> head | _ -> ""

let get_marker_pos marker_len message =
  let message = String.to_list message in
  List.fold_left message ~init:([], 0) ~f:(fun acc c ->
      let marker, pos = (fst acc, snd acc) in
      let len = List.length marker in
      if len < marker_len then (List.rev (c :: List.rev marker), pos + 1)
      else if List.contains_dup marker ~compare:Char.compare then
        match marker with
        | _ :: tail -> (List.rev (c :: List.rev tail), pos + 1)
        | _ -> failwith "impossible"
      else (marker, pos))

let result = get_marker_pos 4 message
let () = printf "Part 1 - %d\n" (snd result)
let result = get_marker_pos 14 message
let () = printf "Part 2 - %d\n" (snd result)
