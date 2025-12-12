open Hardcaml
open Hardware_day4.Hardware

module Simulator = Cyclesim.With_interface(I)(O)

let () = Stdio.printf "part1_ideal=635, part2_ideal=369761800782619\n"