(* open Hardcaml
open Hardware_day6.Hardware

module Simulator = Cyclesim.With_interface(I)(O) *)


let () = Stdio.printf "part1_ideal=3525371263915, part2_ideal=6846480843636\n"

(* let file = "inputs/day6.txt"
let content = In_channel.with_open_text file In_channel.input_lines
let strings = parse_lines content
let () = testbench strings false *)