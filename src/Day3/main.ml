open Hardcaml
open Hardware_day3.Hardware

module Simulator = Cyclesim.With_interface(I)(O)

(* let parse_line line = (* 1-5,8-10 -> [(1, 5); (8, 10)] *)
  List.map (fun s -> 
    String.split_on_char '-' s |> function
      | [l; r] -> (int_of_string l, int_of_string r)
      | _ -> failwith "Invalid line format"
  ) (String.split_on_char ',' line)
;;

Stdio.printf "part1_ideal=19605500130, part2_ideal=36862281418\n"
let file = "inputs/day2.txt"

let content = In_channel.with_open_text file In_channel.input_line
(* let () = List.iter (fun p -> Printf.printf "%d-%d\n" (fst p) (snd p)) (parse_line (Option.get content)) *)
let () = testbench (parse_line (Option.get content)) false *)

let () = Stdio.printf "Day 3 simulation\n"