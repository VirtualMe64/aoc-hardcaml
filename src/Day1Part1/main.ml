open Hardcaml
open Hardware

module Simulator = Cyclesim.With_interface(I)(O)

let testbench input =
  let sim = Simulator.create create in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in
  (* Perform a clock cycle.  Apply the given values to [incr] and [clear].
      Printf the current values of [dout]. *)

  (* Reset simulation *)
  inputs.dir := Bits.gnd;
  inputs.amount := Bits.of_int ~width:16 0;
  inputs.clear := Bits.vdd;
  inputs.valid := Bits.gnd;
  Cyclesim.cycle sim;

  let step ~dir ~amount =
    (* wait for input to be available *)
    while not (Bits.to_bool !(outputs.ready)) do
      inputs.dir := Bits.gnd;
      inputs.amount := Bits.of_int ~width:16 0;
      inputs.clear := Bits.gnd;
      inputs.valid := Bits.gnd;
      Cyclesim.cycle sim;
    done;
    (* provide input *)
    inputs.dir := if dir then Bits.vdd else Bits.gnd;
    inputs.amount := Bits.of_int ~width:16 amount;
    inputs.clear := Bits.gnd;
    inputs.valid := Bits.vdd;
    Cyclesim.cycle sim;
    (* Stdio.printf "rotation='%d', count='%d'\n" (Bits.to_int !(outputs.rotation)) (Bits.to_int !(outputs.count)); *)
  in
  List.iter (fun (dir, amount) -> step ~dir ~amount) input;
  (* allow processing of final element *)
  while not (Bits.to_bool !(outputs.ready)) do
    inputs.dir := Bits.gnd;
    inputs.amount := Bits.of_int ~width:16 0;
    inputs.clear := Bits.gnd;
    inputs.valid := Bits.gnd;
    Cyclesim.cycle sim;
  done;
  Stdio.printf "rotation='%d', count='%d'\n" (Bits.to_int !(outputs.rotation)) (Bits.to_int !(outputs.count));
;;

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

Stdio.printf "Expecting: count=1021\n"
let file = "inputs/day1.txt"
let content = In_channel.with_open_text file In_channel.input_lines
let () = testbench (List.map parse_line content)