open Hardcaml
open Hardcaml.Signal

module type Config = sig
  val max_ranges : int
end

module Make (C : Config) = struct
  module I = struct
    type 'a t =
      { clock  : 'a
      ; clear  : 'a
      ; phase  : 'a
      ; valid  : 'a
      ; number : 'a[@bits 32]
      ; lower  :  'a[@bits 32]
      ; upper  : 'a[@bits 32]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { part1_count : 'a[@bits 32]
      ; part2_count : 'a[@bits 32]
      }
    [@@deriving hardcaml]
  end

  module Processor = struct
    module I = struct
      type 'a t =
        { 
        (* internal signals *)
          clock: 'a
        ; clear: 'a
        (* processing signals *)
        ; phase : 'a
        ; valid : 'a
        (* data passing signals *)
        ; number : 'a[@bits 32]
        ; lower  : 'a[@bits 32]
        ; upper  : 'a[@bits 32]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { phase  : 'a
        ; valid  : 'a
        ; number : 'a[@bits 32]
        ; lower  : 'a[@bits 32]
        ; upper  : 'a[@bits 32]
        ; width  : 'a[@bits 32]
        ; count  : 'a[@bits 32]
        }
      [@@deriving hardcaml]
    end

    let create (i : _ I.t) =
      let smax a b = mux2 (a >=: b) a b in
      let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      let lower_next = wire 32 in
      let lower_reg = reg r_sync lower_next ~enable:(i.valid &: (i.phase ==:. 0)) in
      let upper_next = wire 32 in
      let upper_reg = reg r_sync upper_next ~enable:(i.valid &: (i.phase ==:. 0)) in
      let valid_next = wire 1 in 
      let valid_reg = reg r_sync valid_next ~enable:(i.valid &: (i.phase ==:. 0)) in
      let evicting = i.valid &: (i.phase ==:. 0) &: ((~: valid_reg) |: (i.lower <=: lower_reg)) in
      
      (* Register the input valid and phase for pipelining *)
      let valid_pipe = reg r_sync i.valid in
      let phase_pipe = reg r_sync i.phase in
      let number_pipe = reg r_sync i.number in
      
      let contained = valid_pipe &: (phase_pipe ==:. 1) &: valid_reg &: (number_pipe >=: lower_reg) &: (number_pipe <=: upper_reg) in
    
      valid_next <== (evicting |: valid_reg);
      lower_next <== mux2 (evicting) (i.lower) (lower_reg);
      upper_next <== mux2 (evicting) (i.upper) (upper_reg);

      (* if contained and valid, increment counter *)
      let counter_reg = reg_fb r_sync ~enable:contained ~width:32
        ~f:(fun count -> count +:. 1) in

      { O.phase = phase_pipe (* pass through phase *)
      ; O.valid = (phase_pipe ==:. 1 &: valid_pipe &: ~: contained) |:
                  (i.phase ==:. 0 &: i.valid &: valid_reg)
      ; O.number = number_pipe (* pass through number *)
      ; O.lower = mux2 (evicting) (smax lower_reg (i.upper +:. 1)) (smax i.lower (upper_reg +:. 1))
      ; O.upper = mux2 (evicting) upper_reg i.upper
      ; O.width = mux2 (valid_reg &: (upper_reg >=: lower_reg)) (upper_reg -: lower_reg +:. 1) (of_int ~width:32 0)
      (* ; O.width = lower_reg *)
      ; O.count = counter_reg
      }
    ;;
  end
  
  let create (i : _ I.t) =
    let processor_file = List.fold_left (fun acc idx ->
      let proc_i = Processor.create {
        Processor.I.clock = i.clock;
        Processor.I.clear = i.clear;
        Processor.I.phase = if idx > 0 then (List.hd acc).Processor.O.phase else i.phase;
        Processor.I.valid = if idx > 0 then (List.hd acc).Processor.O.valid else i.valid;
        Processor.I.number = if idx > 0 then (List.hd acc).Processor.O.number else i.number;
        Processor.I.lower = if idx > 0 then (List.hd acc).Processor.O.lower else i.lower;
        Processor.I.upper = if idx > 0 then (List.hd acc).Processor.O.upper else i.upper;
      } in
      proc_i :: acc
    ) [] (List.init C.max_ranges (fun idx -> idx)) in
    {
      O.part1_count = List.fold_left (fun acc proc ->
        acc +: proc.Processor.O.count
      ) (of_int ~width:32 0) processor_file;
      O.part2_count = List.fold_left (fun acc proc ->
        acc +: proc.Processor.O.width
      ) (of_int ~width:32 0) processor_file;
    }
  ;;
end

let testbench ranges ids verbose =
  let cycle_count = ref 0 in
  let module Hw = Make(struct let max_ranges = 200 end) in
  let module Simulator = Cyclesim.With_interface(Hw.I)(Hw.O) in 

  let sim = Simulator.create Hw.create in
  let inputs : _ Hw.I.t = Cyclesim.inputs sim in
  let outputs : _ Hw.O.t = Cyclesim.outputs sim in

  let step_range ~range =
    inputs.clear := Bits.gnd;
    inputs.phase := Bits.gnd;
    inputs.valid := Bits.vdd;
    inputs.number := Bits.of_int ~width:32 0;
    inputs.lower := Bits.of_int ~width:32 (fst range);
    inputs.upper := Bits.of_int ~width:32 (snd range);
    cycle_count := !cycle_count + 1;
    Cyclesim.cycle sim;
    if verbose then
      Stdio.printf "part1_count=%d, part2_count=%d\n" (Bits.to_int !(outputs.part1_count)) (Bits.to_int !(outputs.part2_count));
  in
  let step_id ~id =
    inputs.clear := Bits.gnd;
    inputs.phase := Bits.vdd;
    inputs.valid := Bits.vdd;
    inputs.number := Bits.of_int ~width:32 id;
    inputs.lower := Bits.of_int ~width:32 0;
    inputs.upper := Bits.of_int ~width:32 0;
    cycle_count := !cycle_count + 1;
    Cyclesim.cycle sim;
    if verbose then
      Stdio.printf "part1_count=%d, part2_count=%d\n" (Bits.to_int !(outputs.part1_count)) (Bits.to_int !(outputs.part2_count));
  in

  List.iter (fun range -> step_range ~range:range) ranges;
  List.iter (fun id -> step_id ~id:id) ids;
  for _ = 1 to 200 do
    inputs.clear := Bits.gnd;
    inputs.phase := Bits.vdd;
    inputs.valid := Bits.gnd;
    inputs.number := Bits.of_int ~width:32 0;
    inputs.lower := Bits.of_int ~width:32 0;
    inputs.upper := Bits.of_int ~width:32 0;
    cycle_count := !cycle_count + 1;
    Cyclesim.cycle sim;
  done;
  Stdio.printf "part1_count=%d, part2_count=%d\n" (Bits.to_int !(outputs.part1_count)) (Bits.to_int !(outputs.part2_count));
  Stdio.printf "Total cycles: %d\n" !cycle_count
;;

let test_ranges = [
  (3, 5);
  (10, 14);
  (16, 20);
  (12, 18)
]

let test_numbers = [1; 5; 8; 11; 17; 32]

let%expect_test "test small numbers" =
    (* Construct the simulation and get its input and output ports. *)
    testbench test_ranges test_numbers true;
    [%expect {|
      part1_count=1, part2_count=3
      part1_count=2, part2_count=8
      part1_count=3, part2_count=13
      part1_count=4, part2_count=14
      part1_count=4, part2_count=14
      part1_count=4, part2_count=14
      part1_count=4, part2_count=14
      part1_count=4, part2_count=14
      part1_count=4, part2_count=14
      part1_count=4, part2_count=14
      Total cycles: 9
      |}]