open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; dir : 'a
    ; amount : 'a[@bits 8]
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a (* whether ready to receive input *)
    ; rotation : 'a[@bits 8]
    ; count: 'a[@bits 8]
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
  let gt_100 = amount >: (of_int ~width:8 100) in
  let overflow = rotation >=: (of_int ~width:8 100 -: amount) in
  (
    mux2 gt_100 rotation
      (mux2 overflow
        (rotation +: amount -: of_int ~width:8 100) 
        (rotation +: amount)
      ),
    mux2 (gt_100 |: overflow) (amount -: (of_int ~width:8 100)) amount,
    overflow
  )
;;

let sub_op rotation amount =
  let gt_100 = amount >: (of_int ~width:8 100) in
  let overflow = rotation <: amount in (* if subtracting would be negative *)
  (
    mux2 gt_100 rotation
      (mux2 overflow
        (rotation +: of_int ~width:8 100 -: amount)
        (rotation -: amount)
      ),
    mux2 (gt_100 |: overflow) (amount -: (of_int ~width:8 100)) amount,
    overflow
  )
;;

let create (i : _ I.t) =
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in
    let amount_reg = Always.Variable.reg ~width:8 ~enable:vdd r_sync in
    let direction_reg = Always.Variable.reg ~width:1 ~enable:vdd r_sync in 
    let rotation_reg = Always.Variable.reg ~width:8 ~enable:vdd
      (Reg_spec.override ~clear_to:(of_int ~width:8 50) r_sync) in (* start at 50 *)
    let counter_reg = Always.Variable.reg ~width:8 ~enable:vdd r_sync in
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
          let _overflow = mux2 direction_reg.value add_overflow sub_overflow in
          let gt_100 = amount_reg.value >: (of_int ~width:8 100) in
          [ rotation_reg <-- next_rot
          ; amount_reg <-- next_amt
          ; when_ ((next_rot ==: of_int ~width:8 0) &: (~: gt_100)) [
            counter_reg <-- counter_reg.value +: of_int ~width:8 1
          ]
          ; when_ (~: gt_100) [
             sm.set_next ReadyForInput
          ]
          ])
    ]]);
    { O.rotation = rotation_reg.value
    ; O.count = counter_reg.value
    ; O.ready = sm.is States.ReadyForInput
    }
;;

module Simulator = Cyclesim.With_interface(I)(O)

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
    testbench test_input;
    [%expect {|
      rotation='50', count='0'
      rotation='82', count='0'
      rotation='52', count='0'
      rotation='0', count='1'
      rotation='95', count='1'
      rotation='55', count='1'
      rotation='0', count='2'
      rotation='99', count='2'
      rotation='0', count='3'
      rotation='14', count='3'
      rotation='32', count='3'
      |}]

let (large_numbers) = [
  (true, 250);
  (false, 200);
]

let%expect_test "test large numbers" =
    testbench large_numbers;
    [%expect {|
      rotation='50', count='0'
      rotation='0', count='1'
      rotation='0', count='2'
      |}]