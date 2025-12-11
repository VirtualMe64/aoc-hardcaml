(* https://github.com/janestreet/hardcaml/blob/master/docs/counter_example.md *)
open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; incr : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { dout : 'a[@bits 8]
    }
  [@@deriving hardcaml]
end

let create (i : _ I.t) =
    { O.dout =
        reg_fb
          (Reg_spec.create ~clock:i.clock ~clear:i.clear ())
          ~enable:i.incr
          ~width:8
          ~f:(fun d -> d +:. 1)
    }
  
module Simulator = Cyclesim.With_interface(I)(O)

let%expect_test "testbench" =
    (* Construct the simulation and get its input and output ports. *)
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    (* Perform a clock cycle.  Apply the given values to [incr] and [clear].
       Printf the current values of [dout]. *)
    let step ~clear ~incr =
      inputs.clear := if clear=1 then Bits.vdd else Bits.gnd;
      inputs.incr := if incr=1 then Bits.vdd else Bits.gnd;
      Stdio.printf "dout='%s'\n" (Bits.to_string !(outputs.dout));
      Cyclesim.cycle sim
    in
    (* Run the counter for 6 clock cycles. *)
    step ~clear:0 ~incr:0;
    step ~clear:0 ~incr:1;
    step ~clear:0 ~incr:1;
    step ~clear:1 ~incr:0;
    step ~clear:0 ~incr:0;
    step ~clear:0 ~incr:0;
    [%expect {|
      dout='00000000'
      dout='00000000'
      dout='00000001'
      dout='00000010'
      dout='00000000'
      dout='00000000'
      |}]