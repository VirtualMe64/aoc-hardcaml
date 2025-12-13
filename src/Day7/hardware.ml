open Hardcaml
open Hardcaml.Signal

module type Config = sig
  val bits : int
end

module Make (C : Config) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; input : 'a[@bits (2 * C.bits)]
      ; valid : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { part1_count : 'a[@bits 64]
      ; part2_count : 'a[@bits 64]
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
        ; input: 'a[@bits 2]
        ; valid: 'a
        (* data passing signals *)
        ; left_input: 'a[@bits 64]
        ; right_input: 'a[@bits 64]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { neighbor_output: 'a[@bits 64]
        ; output: 'a[@bits 64]
        ; split_count : 'a[@bits 64]
        }
      [@@deriving hardcaml]
    end

    let create (i : _ I.t) =
      let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      let value_reg = reg_fb r_sync ~enable:vdd ~width:64
        ~f:(fun d ->
          mux2 i.valid (
            mux i.input [
              d;
              of_int ~width:64 0;
              d +:. 1;
              of_int ~width:64 0
            ] +: i.left_input +: i.right_input
          ) d
        ) in
      let split_reg = reg_fb r_sync ~enable:vdd ~width:64
        ~f:(fun d ->
          mux2 (i.valid &: (i.input ==:. 1) &: (value_reg >:. 0)) (d +:. 1) d
        ) in
      { O.neighbor_output = mux2 (i.input ==:. 1) value_reg (of_int ~width:64 0)
      ; O.output = value_reg
      ; O.split_count = split_reg
      }
    ;;
  end
  
  let create (i : _ I.t) =
    let input_wires = Array.init C.bits (fun _ ->
      wire 64;
    ) in
    let processor_file = List.fold_left (fun acc idx ->
      let proc_i = Processor.create {
        Processor.I.clock = i.clock;
        Processor.I.clear = i.clear;
        Processor.I.valid = i.valid;
        Processor.I.input = select i.input (2 * idx + 1) (2 * idx);
        Processor.I.left_input = if idx > 0 then (List.hd acc).Processor.O.neighbor_output else of_int ~width:64 0;
        Processor.I.right_input = if idx < C.bits - 1 then input_wires.(idx + 1) else of_int ~width:64 0;
      } 
      in
        let _ = assign input_wires.(idx) proc_i.Processor.O.neighbor_output
      in
      proc_i :: acc
    ) [] (List.init C.bits (fun idx -> idx))
    in 
    {
      O.part1_count = List.fold_left (fun acc proc ->
        acc +: proc.Processor.O.split_count
      ) (of_int ~width:64 0) processor_file;
      O.part2_count = List.fold_left (fun acc proc ->
        acc +: proc.Processor.O.output
      ) (of_int ~width:64 0) processor_file;
    }
  ;;
end

let testbench input verbose =
  let cycle_count = ref 0 in
  let length = (List.hd input |> String.length) in
  let module Hw = Make(struct let bits = length end) in
  let module Simulator = Cyclesim.With_interface(Hw.I)(Hw.O) in 

  let sim = Simulator.create Hw.create in
  let inputs : _ Hw.I.t = Cyclesim.inputs sim in
  let outputs : _ Hw.O.t = Cyclesim.outputs sim in

  let step ~line =
    let parsed_input = 
      (* map symbols to numbers *)
      (List.init (String.length line) (String.get line)) |>
        List.map (function
          | '.' -> Bits.of_int ~width:2 0
          | '^' -> Bits.of_int ~width:2 1
          | 'S' -> Bits.of_int ~width:2 2
          |  _  -> failwith "invalid input"
      ) |> Bits.concat_msb
    in
    inputs.input := parsed_input;
    inputs.clear := Bits.gnd;
    inputs.valid := Bits.vdd;
    cycle_count := !cycle_count + 1;
    Cyclesim.cycle sim;
    if verbose then
      Stdio.printf "part1_count=%d, part2_count=%d\n" (Bits.to_int !(outputs.part1_count)) (Bits.to_int !(outputs.part2_count));
  in
  List.iter (fun line -> step ~line:line) input;
  Stdio.printf "part1_count=%d, part2_count=%d\n" (Bits.to_int !(outputs.part1_count)) (Bits.to_int !(outputs.part2_count));
  Stdio.printf "Total cycles: %d\n" !cycle_count
;;

let test_input = [
  ".......S......."
; "..............."
; ".......^......."
; "..............."
; "......^.^......"
; "..............."
; ".....^.^.^....."
; "..............."
; "....^.^...^...."
; "..............."
; "...^.^...^.^..."
; "..............."
; "..^...^.....^.."
; "..............."
; ".^.^.^.^.^...^."
; "..............."
]

let%expect_test "test small numbers" =
    (* Construct the simulation and get its input and output ports. *)
    testbench test_input true;
    [%expect {|
      part1_count=0, part2_count=1
      part1_count=0, part2_count=1
      part1_count=1, part2_count=2
      part1_count=1, part2_count=2
      part1_count=3, part2_count=4
      part1_count=3, part2_count=4
      part1_count=6, part2_count=8
      part1_count=6, part2_count=8
      part1_count=9, part2_count=13
      part1_count=9, part2_count=13
      part1_count=13, part2_count=20
      part1_count=13, part2_count=20
      part1_count=16, part2_count=26
      part1_count=16, part2_count=26
      part1_count=21, part2_count=40
      part1_count=21, part2_count=40
      part1_count=21, part2_count=40
      Total cycles: 16
      |}]