open Hardcaml
open Hardware_day3.Hardware

module Simulator = Cyclesim.With_interface(I)(O)

let parse_line line =
  String.trim line
;;

Stdio.printf "part1_ideal=16887, part2_ideal=167302518850275\n"
let file = "inputs/day3.txt"

let content = In_channel.with_open_text file In_channel.input_lines
let () = testbench (List.map parse_line content) false
(* let () = List.iter (fun line -> Stdio.printf "line=%s\n" (parse_line line)) content *)