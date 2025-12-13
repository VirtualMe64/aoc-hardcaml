open Hardcaml
open Hardware_day4.Hardware

module Simulator = Cyclesim.With_interface(I)(O)

let parse_lines lines =
  List.map String.trim lines
;;

Stdio.printf "part1_ideal=1376, part2_ideal=8587\n"

let file = "inputs/day4.txt"
let content = In_channel.with_open_text file In_channel.input_lines
let strings = parse_lines content
let () = testbench strings false