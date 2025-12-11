open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; lower : 'a[@bits 32]
    ; upper : 'a[@bits 32]
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a (* whether ready to receive input *)
    ; count : 'a[@bits 32]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t = 
    | ReadyForInput
    | Processing
  [@@deriving sexp_of, compare ~localize, enumerate]
end

(* idea: for each range, first extract the base 10 digits into digit_regfile *)
(* check for equality with length check + concat + xor *)
(* increment efficiently by scanning *)
(* states are ReadyForInput -> ExtractingDigits -> CheckingEquality -> Incrementing *)
let create (i : _ I.t) =
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in
    let curr_reg = Always.Variable.reg ~width:32 ~enable:vdd r_sync in
    let upper_reg = Always.Variable.reg ~width:32 ~enable:vdd r_sync in
    let _digit_regfile = List.init 16 (fun _ -> Always.Variable.reg ~width:4 ~enable:vdd r_sync) in
    let counter_reg = Always.Variable.reg ~width:32 ~enable:vdd r_sync in

    Always.(
      compile [
        sm.switch [
          (ReadyForInput, 
          [ curr_reg <-- i.lower
          ; upper_reg <-- i.upper
          ; when_ i.valid [
            sm.set_next Processing
          ]
          ]);
          (Processing, 
          [ curr_reg <-- curr_reg.value +:. 1
          ; when_ (curr_reg.value >=: upper_reg.value) [
             sm.set_next ReadyForInput
          ]
          ])
    ]]);
    { O.count = counter_reg.value
    ; O.ready = sm.is States.ReadyForInput
    }
;;

module Simulator = Cyclesim.With_interface(I)(O)

let testbench input verbose =
  let sim = Simulator.create create in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in
  (* Perform a clock cycle.  Apply the given values to [incr] and [clear].
      Printf the current values of [dout]. *)

  (* Reset simulation *)
  inputs.lower := Bits.of_int ~width:32 0;
  inputs.upper := Bits.of_int ~width:32 0;
  inputs.clear := Bits.vdd;
  inputs.valid := Bits.gnd;
  Cyclesim.cycle sim;

  let step ~lower ~upper =
    (* wait for input to be available *)
    while not (Bits.to_bool !(outputs.ready)) do
      inputs.lower := Bits.of_int ~width:32 0;
      inputs.upper := Bits.of_int ~width:32 0;
      inputs.clear := Bits.gnd;
      inputs.valid := Bits.gnd;
      Cyclesim.cycle sim;
    done;
    (* provide input *)
    inputs.lower := Bits.of_int ~width:32 lower;
    inputs.upper := Bits.of_int ~width:32 upper;
    inputs.clear := Bits.gnd;
    inputs.valid := Bits.vdd;
    Cyclesim.cycle sim;
    if verbose then
      Stdio.printf "count=%d\n" (Bits.to_int !(outputs.count));
  in
  List.iter (fun (lower, upper) -> step ~lower ~upper) input;
  (* allow processing of final element *)
  while not (Bits.to_bool !(outputs.ready)) do
    inputs.lower := Bits.of_int ~width:32 0;
    inputs.upper := Bits.of_int ~width:32 0;
    inputs.clear := Bits.gnd;
    inputs.valid := Bits.gnd;
    Cyclesim.cycle sim;
  done;
  Stdio.printf "count=%d\n" (Bits.to_int !(outputs.count));
;;

let test_input = [
  (11, 22);
  (* (95, 115);
  (998, 1012);
  (1188511880, 1188511890);
  (222220, 222224);
  (1698522, 1698528);
  (446443, 446449);
  (38593856, 38593862);
  (565653, 565659);
  (824824821, 824824827);
  (2121212118, 2121212124); *)
]

let%expect_test "test small numbers" =
    (* Construct the simulation and get its input and output ports. *)
    testbench test_input true;
    [%expect {|
      count=0
      count=12
      count=33
      count=48
      count=59
      count=64
      count=71
      count=78
      count=85
      count=92
      count=99
      count=106
      |}]