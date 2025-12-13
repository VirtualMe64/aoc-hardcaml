open Hardware_day7.Hardware

let parse_lines lines =
  List.map String.trim lines
;;

Stdio.printf "part1_ideal=1667, part2_ideal=62943905501815\n"

let file = "inputs/day7.txt"
let content = In_channel.with_open_text file In_channel.input_lines

let strings = parse_lines content
let () = testbench strings false