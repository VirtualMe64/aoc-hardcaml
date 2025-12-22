(* open Hardcaml *)
open Hardware_day6.Hardware

(* trim s then append a space character *)
let pad_string s = 
  let s = String.trim s in
  s ^ " "

let () = Stdio.printf "part1_ideal=3525371263915, part2_ideal=6846480843636\n"

let file = "inputs/day6.txt"
let content = In_channel.with_open_text file In_channel.input_lines
let strings = List.map pad_string content
let () = testbench strings false