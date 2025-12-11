open Hardcaml
open Hardware_day1.Hardware

module Simulator = Cyclesim.With_interface(I)(O)

let parse_line line =
  let dir_char = String.get line 0 in
  let dir = match dir_char with
    | 'L' -> false
    | 'R' -> true
    | _ -> failwith "Invalid direction character"
  in
  let amount_str = String.sub line 1 (String.length line - 1) in
  let amount = int_of_string amount_str in
  (dir, amount)
;;

Stdio.printf "Expecting: part1=1021, part2=5933\n"
let file = "inputs/day1.txt"
let content = In_channel.with_open_text file In_channel.input_lines
let () = testbench (List.map parse_line content) false