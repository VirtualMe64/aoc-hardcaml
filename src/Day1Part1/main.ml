open Hardcaml
open Hardware

module Simulator = Cyclesim.With_interface(I)(O)

let test_input = [
  (false, 68);
  (false, 30);
  (true, 48);
  (false, 5);
  (true, 60);
  (false, 55);
  (false, 1);
  (false, 99);
  (true, 14);
  (false, 82);
]

let testbench input =
  let sim = Simulator.create create in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in
  (* Perform a clock cycle.  Apply the given values to [incr] and [clear].
      Printf the current values of [dout]. *)

  (* Reset simulation *)
  inputs.dir := Bits.gnd;
  inputs.amount := Bits.of_int ~width:8 0;
  inputs.clear := Bits.vdd;
  inputs.valid := Bits.gnd;
  Cyclesim.cycle sim;

  let step ~dir ~amount =
    (* wait for input to be available *)
    while not (Bits.to_bool !(outputs.ready)) do
      inputs.dir := Bits.gnd;
      inputs.amount := Bits.of_int ~width:8 0;
      inputs.clear := Bits.gnd;
      inputs.valid := Bits.gnd;
      Cyclesim.cycle sim;
    done;
    (* provide input *)
    inputs.dir := if dir then Bits.vdd else Bits.gnd;
    inputs.amount := Bits.of_int ~width:8 amount;
    inputs.clear := Bits.gnd;
    inputs.valid := Bits.vdd;
    Cyclesim.cycle sim;
    Stdio.printf "rotation='%d', count='%d'\n" (Bits.to_int !(outputs.rotation)) (Bits.to_int !(outputs.count));
  in
  List.iter (fun (dir, amount) -> step ~dir ~amount) input;
  (* allow processing of final element *)
  while not (Bits.to_bool !(outputs.ready)) do
    inputs.dir := Bits.gnd;
    inputs.amount := Bits.of_int ~width:8 0;
    inputs.clear := Bits.gnd;
    inputs.valid := Bits.gnd;
    Cyclesim.cycle sim;
  done;
  Stdio.printf "rotation='%d', count='%d'\n" (Bits.to_int !(outputs.rotation)) (Bits.to_int !(outputs.count));
;;

let () = testbench test_input