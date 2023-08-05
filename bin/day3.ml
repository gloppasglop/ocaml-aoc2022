open Base
open Stdio

let file = "day3.txt"

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

let split s =
  let l = String.length s in
  let left = String.drop_suffix s (l / 2) in
  let right = String.drop_prefix s (l / 2) in
  (left, right)

let rec char_in_common s1 s2 =
  if equal_string s1 "" then '@'
  else if String.contains s2 s1.[0] then s1.[0]
  else char_in_common (String.drop_prefix s1 1) s2

(* let () = List.iter ~f:(printf "%s\n") data *)
let result =
  let data' = List.map ~f:split data in
  let commmon_char line = char_in_common (fst line) (snd line) in
  let char_to_prio c =
    if Char.to_int c > Char.to_int 'a' then Char.to_int c - Char.to_int 'a' + 1
    else Char.to_int c - Char.to_int 'A' + 27
  in
  List.fold_left
    (List.map ~f:(fun line -> line |> commmon_char |> char_to_prio) data')
    ~init:0 ~f:( + )

let () = printf "Part 1 - %d\n" result

let rec chars_in_common s1 s2 =
  if equal_string s1 "" then ""
  else if String.contains s2 s1.[0] then
    Char.to_string s1.[0] ^ chars_in_common (String.drop_prefix s1 1) s2
  else chars_in_common (String.drop_prefix s1 1) s2

let chars_in_common3 (s1, s2, s3) =
  let s1' = chars_in_common s1 s2 in
  chars_in_common s1' s3

let rec group_b_3 l =
  match l with
  | a :: b :: c :: rest -> (a, b, c) :: group_b_3 rest
  | [] -> []
  | _ -> failwith "Not enough data"

let result =
  let data' = group_b_3 data in
  let commmon_char item = String.get (chars_in_common3 item) 0 in
  let char_to_prio c =
    if Char.to_int c > Char.to_int 'a' then Char.to_int c - Char.to_int 'a' + 1
    else Char.to_int c - Char.to_int 'A' + 27
  in
  List.fold_left
    (List.map ~f:(fun item -> item |> commmon_char |> char_to_prio) data')
    ~init:0 ~f:( + )

let () = printf "Part 2 - %d\n" result
