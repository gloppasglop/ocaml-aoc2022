open Base
open Stdio

let file = "day5.txt"

type aocdata = {
  section : string;
  instructions : string list;
  stacks : string list;
}

let _pp_aocdata r =
  printf "{ section: %s" r.section;
  print_string "  instructions:\n";
  List.iter r.instructions ~f:(printf "        %s\n");
  print_string "  stack:\n";
  List.iter r.stacks ~f:(printf "        %s\n")

let data = In_channel.read_lines file

let parse_data data =
  (* let update_acc line acc =
     if equal_string line "" then
       match acc with
       | { section = _; instructions = _; stack = st } ->
           { section = "instructions"; instructions = []; stack = st }
     else { section = "ddd"; instructions = []; stack = [] }
  *)
  let update_acc line acc =
    match String.prefix line 2 with
    | "mo" -> (
        match acc with
        | { section = _; instructions; stacks = st } ->
            {
              section = "instructions";
              instructions = line :: instructions;
              stacks = st;
            })
    | " 1" -> (
        match acc with
        | { section = _; instructions; stacks = st } ->
            { section = "dummy"; instructions; stacks = st })
    | "" -> (
        match acc with
        | { section = _; instructions; stacks = st } ->
            { section = "dummy"; instructions; stacks = st })
    | _ -> (
        match acc with
        | { section = _; instructions; stacks = st } ->
            { section = "stack"; instructions; stacks = line :: st })
  in
  let rec parse_data_aux data acc =
    match data with
    | head :: tail -> parse_data_aux tail (update_acc head acc)
    | [] -> acc
  in
  parse_data_aux data { section = "stack"; instructions = []; stacks = [] }

let parsed_data = parse_data data

type movement = { quantity : int; fromStack : int; toStack : int }

let _pp_movement move =
  printf "move %d %d -> %d\n" move.quantity move.fromStack move.toStack

let get_moves data =
  let parse_move move =
    match String.split ~on:' ' move with
    | [ _; quantity; _; fromStack; _; toStack ] ->
        {
          quantity = Int.of_string quantity;
          fromStack = Int.of_string fromStack - 1;
          (* index start at 1 in puzzle input *)
          toStack = Int.of_string toStack - 1;
        }
    | _ -> failwith "Impossible move"
  in
  List.map data ~f:parse_move

let get_stacks data =
  (* Create an list of empty stacks. *)
  let stacks =
    match data with
    | head :: _ ->
        List.map (String.split ~on:' ' head) ~f:(fun _ -> Stack.create ())
    | [] -> []
  in
  let get_element i line =
    let elt =
      String.drop_prefix (String.prefix line ((4 * i) + 2)) ((4 * i) + 1)
    in
    if equal_string elt " " then None else Some elt
  in
  let rec populate_stacks data =
    match data with
    | head :: tail ->
        List.iteri stacks ~f:(fun i stack ->
            let elt = get_element i head in
            match elt with Some e -> Stack.push stack e | None -> ());
        populate_stacks tail
    | [] -> ()
  in
  populate_stacks data;
  stacks

let moves = List.rev (get_moves parsed_data.instructions)

let result =
  let stacks_as_list = get_stacks parsed_data.stacks in
  let stacks = List.to_array stacks_as_list in
  let () =
    List.iter moves ~f:(fun move ->
        let rec move_crates qty =
          if qty > 0 then (
            match Stack.pop stacks.(move.fromStack) with
            | None -> ()
            | Some s ->
                Stack.push stacks.(move.toStack) s;
                move_crates (qty - 1))
          else ()
        in
        move_crates move.quantity)
  in
  List.fold_left stacks_as_list ~init:"" ~f:(fun acc stack ->
      acc ^ match Stack.top stack with Some s -> s | None -> "")

let () = printf "Part 1 : %s\n" result

let result =
  let stacks_as_list = get_stacks parsed_data.stacks in
  let stacks = List.to_array stacks_as_list in
  let () =
    List.iter moves ~f:(fun move ->
        let grabbed_crates =
          let rec move_crates qty =
            if qty > 0 then
              match Stack.pop stacks.(move.fromStack) with
              | None -> []
              | Some s -> s :: move_crates (qty - 1)
            else []
          in
          move_crates move.quantity
        in
        List.iter (List.rev grabbed_crates)
          ~f:(Stack.push stacks.(move.toStack)))
  in
  List.fold_left stacks_as_list ~init:"" ~f:(fun acc stack ->
      acc ^ match Stack.top stack with Some s -> s | None -> "")

let () = printf "Part 2 : %s\n" result
