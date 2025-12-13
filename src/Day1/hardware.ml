open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; dir : 'a
    ; amount : 'a[@bits 16]
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a (* whether ready to receive input *)
    ; rotation : 'a[@bits 16]
    ; part1_count: 'a[@bits 16]
    ; part2_count: 'a[@bits 16]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t = 
    | ReadyForInput
    | Processing
  [@@deriving sexp_of, compare ~localize, enumerate]
end

(* returns new value (for rotation/amount) and whether there was overflow *)
let add_op rotation amount = 
  let gt_100 = amount >: (of_int ~width:16 100) in
  let overflow = rotation >=: (of_int ~width:16 100 -: amount) in
  (
    mux2 gt_100 rotation
      (mux2 overflow
        (rotation +: amount -: of_int ~width:16 100) 
        (rotation +: amount)
      ),
    mux2 (gt_100 |: overflow) (amount -: (of_int ~width:16 100)) amount,
    overflow
  )
;;

let sub_op rotation amount =
  let gt_100 = amount >: (of_int ~width:16 100) in
  let overflow = rotation <: amount in (* if subtracting would be negative *)
  (
    mux2 gt_100 rotation
      (mux2 overflow
        (rotation +: of_int ~width:16 100 -: amount)
        (rotation -: amount)
      ),
    mux2 (gt_100 |: overflow) (amount -: (of_int ~width:16 100)) amount,
    overflow
  )
;;

let create (i : _ I.t) =
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in
    let amount_reg = Always.Variable.reg ~width:16 ~enable:vdd r_sync in
    let direction_reg = Always.Variable.reg ~width:1 ~enable:vdd r_sync in 
    let rotation_reg = Always.Variable.reg ~width:16 ~enable:vdd
      (Reg_spec.override ~clear_to:(of_int ~width:16 50) r_sync) in (* start at 50 *)
    let part1_counter_reg = Always.Variable.reg ~width:16 ~enable:vdd r_sync in
    let part2_counter_reg = Always.Variable.reg ~width:16 ~enable:vdd r_sync in
    Always.(
      compile [
        sm.switch [
          (ReadyForInput, 
          [ amount_reg <-- i.amount
          ; direction_reg <-- i.dir
          ; when_ i.valid [
            sm.set_next Processing
          ]
          ]);
          (Processing, 
          let add_rot, add_amt, add_overflow = add_op rotation_reg.value amount_reg.value in
          let sub_rot, sub_amt, sub_overflow = sub_op rotation_reg.value amount_reg.value in
          let next_rot = mux2 direction_reg.value add_rot sub_rot in
          let next_amt = mux2 direction_reg.value add_amt sub_amt in
          let overflow = mux2 direction_reg.value add_overflow sub_overflow in
          let gt_100 = amount_reg.value >: (of_int ~width:16 100) in
          [ rotation_reg <-- next_rot
          ; amount_reg <-- next_amt
          ; when_ ((next_rot ==: of_int ~width:16 0) &: (~: gt_100)) [
            part1_counter_reg <-- part1_counter_reg.value +: of_int ~width:16 1
          ]
          ; when_ ((next_rot ==: of_int ~width:16 0) |: gt_100 |:
            (overflow &: (rotation_reg.value <>: of_int ~width:16 0))) [
            part2_counter_reg <-- part2_counter_reg.value +: of_int ~width:16 1
          ]
          ; when_ (~: gt_100) [
             sm.set_next ReadyForInput
          ]
          ])
    ]]);
    { O.rotation = rotation_reg.value
    ; O.part1_count = part1_counter_reg.value
    ; O.part2_count = part2_counter_reg.value
    ; O.ready = sm.is States.ReadyForInput
    }
;;

module Simulator = Cyclesim.With_interface(I)(O)

let testbench input verbose =
  let cycle_count = ref 0 in
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
      cycle_count := !cycle_count + 1;
      Cyclesim.cycle sim;
    done;
    (* provide input *)
    inputs.dir := if dir then Bits.vdd else Bits.gnd;
    inputs.amount := Bits.of_int ~width:16 amount;
    inputs.clear := Bits.gnd;
    inputs.valid := Bits.vdd;
    cycle_count := !cycle_count + 1;
    Cyclesim.cycle sim;
    if verbose then
      Stdio.printf "rotation=%d, part1=%d, part2=%d\n" (Bits.to_int !(outputs.rotation)) (Bits.to_int !(outputs.part1_count)) (Bits.to_int !(outputs.part2_count));
  in
  List.iter (fun (dir, amount) -> step ~dir ~amount) input;
  (* allow processing of final element *)
  while not (Bits.to_bool !(outputs.ready)) do
    inputs.dir := Bits.gnd;
    inputs.amount := Bits.of_int ~width:16 0;
    inputs.clear := Bits.gnd;
    inputs.valid := Bits.gnd;
    cycle_count := !cycle_count + 1;
    Cyclesim.cycle sim;
  done;
  Stdio.printf "rotation=%d, part1=%d, part2=%d\n" (Bits.to_int !(outputs.rotation)) (Bits.to_int !(outputs.part1_count)) (Bits.to_int !(outputs.part2_count));
  Stdio.printf "Total cycles: %d\n" !cycle_count
;;

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

let%expect_test "test small numbers" =
    (* Construct the simulation and get its input and output ports. *)
    testbench test_input true;
    [%expect {|
      rotation=50, part1=0, part2=0
      rotation=82, part1=0, part2=1
      rotation=52, part1=0, part2=1
      rotation=0, part1=1, part2=2
      rotation=95, part1=1, part2=2
      rotation=55, part1=1, part2=3
      rotation=0, part1=2, part2=4
      rotation=99, part1=2, part2=4
      rotation=0, part1=3, part2=5
      rotation=14, part1=3, part2=5
      rotation=32, part1=3, part2=6
      Total cycles: 20
      |}]

let (large_numbers) = [
  (true, 250);
  (false, 200);
]

let%expect_test "test large numbers" =
    testbench large_numbers true;
    [%expect {|
      rotation=50, part1=0, part2=0
      rotation=0, part1=1, part2=3
      rotation=0, part1=2, part2=5
      Total cycles: 7
      |}]