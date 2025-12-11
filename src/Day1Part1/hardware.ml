open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; reset : 'a
    ; dir : 'a
    ; amount : 'a[@bits 8]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { rotation : 'a[@bits 8]
    ; count: 'a[@bits 8]
    }
  [@@deriving hardcaml]
end

let add_op d amount = let max_without_overflow = (of_int ~width:8 99) -: amount in
  mux2 (d >: max_without_overflow) (* if sum would be >= 100 *)
    (d +: amount -: of_int ~width:8 100)
    (d +: amount)

let sub_op d amount =
  mux2 (d <: amount) (* if subtraction would be negative *)
    (d +: of_int ~width:8 100 -: amount)
    (d -: amount)

let create (i : _ I.t) =
    let rotation_reg =
      reg_fb
        (Reg_spec.override ~clear_to:(of_int ~width:8 50)
          (Reg_spec.create ~clock:i.clock ~clear:i.reset ()))
        ~enable:vdd
        ~width:8
        ~f:(fun d -> 
              mux2 i.dir
                (add_op d i.amount)
                (sub_op d i.amount)
        ) 
       in
    let count_reg =
      reg_fb
        (Reg_spec.create ~clock:i.clock ~clear:i.reset ())
        ~enable:vdd
        ~width:8
        ~f:(fun d -> mux2 (rotation_reg ==: of_int ~width:8 0)
                      (d +: of_int ~width:8 1)
                      d)
       in
    { O.rotation = rotation_reg
    ; O.count = count_reg
    }

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
  inputs.reset := Bits.vdd;
  Cyclesim.cycle sim;

  Stdio.printf "rotation='%d', count='%d'\n" (Bits.to_int !(outputs.rotation)) (Bits.to_int !(outputs.count));
  let step ~dir ~amount =
    inputs.dir := if dir then Bits.vdd else Bits.gnd;
    inputs.amount := Bits.of_int ~width:8 amount;
    inputs.reset := Bits.gnd;
    Cyclesim.cycle sim;
    Stdio.printf "rotation='%d', count='%d'\n" (Bits.to_int !(outputs.rotation)) (Bits.to_int !(outputs.count));

  in
  (* Run the counter for 6 clock cycles. *)
  List.iter (fun (dir, amount) -> step ~dir ~amount) input;
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
      rotation='0', count='0'
      rotation='95', count='1'
      rotation='55', count='1'
      rotation='0', count='1'
      rotation='99', count='2'
      rotation='0', count='2'
      rotation='14', count='3'
      rotation='32', count='3'
      |}]
