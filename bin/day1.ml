open Base
open Stdio

let file = "day1_1.txt"
let calories = In_channel.read_lines file

let rec group_by_elfe calories result =
  match calories with
  | head :: tail -> (
      match head with
      | "" -> group_by_elfe tail (0 :: result)
      | _ ->
          let result =
            match result with
            | rh :: rt -> (rh + Int.of_string head) :: rt
            | [] -> [ 0 ]
          in
          group_by_elfe tail result)
  | [] -> result

let result = group_by_elfe calories [ 0 ]
let () = printf "Day 1 - 1 : %d\n" (List.fold_right result ~f:max ~init:0)

let rec top3 list acc =
  match list with
  | head :: tail -> (
      match acc with
      | a, b, _ when head > a -> top3 tail (head, a, b)
      | a, b, _ when head > b -> top3 tail (a, head, b)
      | a, b, c when head > c -> top3 tail (a, b, head)
      | _ -> top3 tail acc)
  | [] -> acc

let () =
  let a, b, c = top3 result (0, 0, 0) in
  printf "Day 1 - 2 : %d\n" (a + b + c)
