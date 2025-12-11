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

let testbench () =
  let sim = Simulator.create create in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in
  (* Perform a clock cycle.  Apply the given values to [incr] and [clear].
      Printf the current values of [dout]. *)

  (* Reset simulation *)
  inputs.dir := Bits.gnd;
  inputs.amount := Bits.of_int ~width:8 0;
  inputs.reset := Bits.vdd;
  Cyclesim.cycle sim;

  Stdio.printf "rotation='%d', count='%d'\n" (Bits.to_int !(outputs.rotation)) (Bits.to_int !(outputs.count));

  let step ~dir ~amount =
    inputs.dir := if dir then Bits.vdd else Bits.gnd;
    inputs.amount := Bits.of_int ~width:8 amount;
    inputs.reset := Bits.gnd;
    Cyclesim.cycle sim;
    Stdio.printf "rotation='%d', count='%d'\n" (Bits.to_int !(outputs.rotation)) (Bits.to_int !(outputs.count))
  in
  (* Run the counter for 6 clock cycles. *)
  List.iter (fun (dir, amount) -> step ~dir ~amount) test_input;
;;

let () = testbench ()