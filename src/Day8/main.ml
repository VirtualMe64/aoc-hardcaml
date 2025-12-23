open Hardware_day8.Hardware

let parse_line line =
  let split_line = String.split_on_char ',' line in
  match split_line with
  | [a ; b ; c] -> (int_of_string a, int_of_string b, int_of_string c)
  | _ -> failwith "parse_line: invalid line"
;;

Stdio.printf "part1_ideal=163548, part2_ideal=772452514\n"

let file = "inputs/day8.txt"
let content = In_channel.with_open_text file In_channel.input_lines
let strings = List.map parse_line content
let () = testbench strings false