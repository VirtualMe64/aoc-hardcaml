open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; next_input : 'a
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a (* whether output is valid/ready to receive input *)
    ; count : 'a[@bits 64]
    }
  [@@deriving hardcaml]
end


(* idea: only solve part 1 *)
(* keep memory of size 3 * width *)
(* circular array + update as you go *)
(* only need 3 bits per cell (1 for if @, 2 for nbr count -- if >= 4 doesn't matter) *)
let create (i : _ I.t) =
    let mem = 
;;

module Simulator = Cyclesim.With_interface(I)(O)

let testbench input verbose =
  let cycle_count = ref 0 in
  let sim = Simulator.create create in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in

  let stream ~number =
    (* indicate new input *)
    inputs.remaining := Bits.of_int ~width:8 0;
    inputs.number := Bits.of_int ~width:4 0;
    inputs.clear := Bits.gnd;
    inputs.valid := Bits.gnd;
    inputs.new_input := Bits.vdd;
    cycle_count := !cycle_count + 1;
    Cyclesim.cycle sim;

    String.iteri (fun i c ->
      let digit = Char.code c - Char.code '0' in
      let remaining = String.length number - i - 1 in
      inputs.remaining := Bits.of_int ~width:8 remaining;
      inputs.number := Bits.of_int ~width:4 digit;
      inputs.clear := Bits.gnd;
      inputs.valid := Bits.vdd;
      inputs.new_input := Bits.gnd;
      cycle_count := !cycle_count + 1;
      Cyclesim.cycle sim;
      (* Stdio.printf "result=%d\n" (bcd_to_int (Bits.to_int !(outputs.result_bcd))); *)
    ) number;
    while (not (Bits.to_bool !(outputs.ready))) do
      inputs.remaining := Bits.of_int ~width:8 0;
      inputs.number := Bits.of_int ~width:4 0;
      inputs.clear := Bits.gnd;
      inputs.valid := Bits.gnd;
      inputs.new_input := Bits.gnd;
      cycle_count := !cycle_count + 1;
      Cyclesim.cycle sim;
      (* Stdio.printf "result=%d\n" (bcd_to_int (Bits.to_int !(outputs.result_bcd))); *)
    done;
    if verbose then
      Stdio.printf "part1_count=%d, part2_count=%d\n" (Bits.to_int !(outputs.part1_count)) (Bits.to_int !(outputs.part2_count));
  in
  List.iter (fun line -> stream ~number:line) input;
  Stdio.printf "part1_count=%d, part2_count=%d\n" (Bits.to_int !(outputs.part1_count)) (Bits.to_int !(outputs.part2_count));
  Stdio.printf "Total cycles: %d\n" !cycle_count
;;

let test_input = [
  "0011011110";
  "1110101011";
  "1111101011";
  "1011110010";
  "1101111011";
  "0111111101";
  "0101010111";
  "1011101111";
  "0111111110";
  "1010111010";
]

let%expect_test "test circuit" =
    (* Construct the simulation and get its input and output ports. *)
    testbench test_input true;
    [%expect {|
      part1_count=98, part2_count=987654321111
      part1_count=187, part2_count=1798765432230
      part1_count=265, part2_count=2232999666508
      part1_count=357, part2_count=3121910778619
      part1_count=357, part2_count=3121910778619
      Total cycles: 116
      |}]