open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; number : 'a[@bits 4]
    ; remaining : 'a[@bits 8] (* number of characters remaining after current *)
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a (* whether output is valid/ready to receive input *)
    ; result_bcd : 'a[@bits 48] (* 12 digits in binary *)
    }
  [@@deriving hardcaml]
end

module Processor = struct
  module I = struct
    type 'a t =
      { 
      (* internal signals *)
        index: 'a[@bits 4] (* internal index of the processor *)
      ; clock: 'a
      ; clear: 'a
      (* processing signals *)
      ; valid: 'a
      ; number: 'a[@bits 4] (* input number to process *)
      ; min_index: 'a[@bits 4] (* minimum index for this number to be processed *)
      ; zeroing: 'a (* whether this number is zeroing *)
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { value: 'a[@bits 4] (* current value *) 
      ; valid: 'a           (* pass through *)
      ; number: 'a[@bits 4] (* pass through *)
      ; min_index: 'a[@bits 4] (* pass through *)
      ; zeroing: 'a (* whether this number is now zeroing *)
      ; ready: 'a   (* whether the output is ready*)
      }
    [@@deriving hardcaml]
  end

  let create (i : _ I.t) =
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let input_active = i.index >=: i.min_index in
    let valid_reg = reg_fb r_sync ~enable:vdd ~width:1 ~f:(fun _ -> i.valid) in
    let number_reg = reg_fb r_sync ~enable:vdd ~width:4 ~f:(fun _ -> i.number) in
    let min_index_reg = reg_fb r_sync ~enable:vdd ~width:4 ~f:(fun _ -> i.min_index) in
    let value_reg = reg_fb r_sync ~enable:vdd ~width:4
      ~f:(fun d ->
        let replacing = i.number >: d in
        mux2 i.zeroing (of_int ~width:4 0) (
          mux2 (input_active &: replacing) i.number d
        )
      )
    in
    let zeroing_reg = reg_fb r_sync ~enable:vdd ~width:1
      ~f:(fun _ ->
        i.zeroing |: (input_active &: (i.number >: value_reg))
      )
    in
    let ready_reg = reg_fb r_sync ~enable:vdd ~width:1 ~f:(fun _ -> ~:valid_reg) in
    { O.value = value_reg
    ; O.valid = valid_reg
    ; O.number = number_reg
    ; O.min_index = min_index_reg
    ; O.zeroing = zeroing_reg
    ; O.ready = ready_reg
    }
  ;;
end

(* streaming processor *)
(* inputs passed between *)
let create (i : _ I.t) =
    let processor_file =
      List.fold_left (fun acc idx ->
        let proc_i = Processor.create {
          index = of_int ~width:4 idx;
          clock = i.clock;
          clear = i.clear;
          valid = if idx == 0 then i.valid else (List.hd acc).Processor.O.valid;
          number = if idx == 0 then i.number else (List.hd acc).Processor.O.number;
          min_index = if idx == 0 then mux2
              (i.remaining >:. 11) (of_int ~width:4 0) ((of_int ~width:4 11) -: select i.remaining 3 0)
            else
              (List.hd acc).Processor.O.min_index;
          zeroing = if idx == 0 then gnd else (List.hd acc).Processor.O.zeroing;
        } in
        proc_i :: acc
      ) [] (List.init 12 (fun i -> i))
    in
    {
      O.ready = List.fold_left (fun acc p -> acc &: p.Processor.O.ready) vdd processor_file;
      O.result_bcd = Signal.concat_msb (
        List.map (fun p -> p.Processor.O.value) (List.rev processor_file)
      )
    }
;;

module Simulator = Cyclesim.With_interface(I)(O)

let testbench input verbose =
  let rec bcd_to_int n =
    if n = 0 then 0
    else (n land 0xF) + (10 * bcd_to_int (n lsr 4))
  in
  let cycle_count = ref 0 in
  let sim = Simulator.create create in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in

  (* Reset simulation *)
  inputs.remaining := Bits.of_int ~width:8 0;
  inputs.number := Bits.of_int ~width:4 0;
  inputs.clear := Bits.vdd;
  inputs.valid := Bits.gnd;
  Cyclesim.cycle sim;

  let stream ~number =
    String.iteri (fun i c ->
      (* provide input *)
      let digit = Char.code c - Char.code '0' in
      let remaining = String.length number - i - 1 in
      inputs.remaining := Bits.of_int ~width:8 remaining;
      inputs.number := Bits.of_int ~width:4 digit;
      inputs.clear := Bits.gnd;
      inputs.valid := Bits.vdd;
      cycle_count := !cycle_count + 1;
      Cyclesim.cycle sim;
      (* Stdio.printf "result=%d\n" (bcd_to_int (Bits.to_int !(outputs.result_bcd))); *)
    ) number;
    while (not (Bits.to_bool !(outputs.ready))) do
      inputs.remaining := Bits.of_int ~width:8 0;
      inputs.number := Bits.of_int ~width:4 0;
      inputs.clear := Bits.gnd;
      inputs.valid := Bits.gnd;
      cycle_count := !cycle_count + 1;
      Cyclesim.cycle sim;
      (* Stdio.printf "result=%d\n" (bcd_to_int (Bits.to_int !(outputs.result_bcd))); *)
    done;
    if verbose then
      Stdio.printf "count=%d\n" (bcd_to_int (Bits.to_int !(outputs.result_bcd)));
  in
  List.iter (fun line -> stream ~number:line) input;
  Stdio.printf "Total cycles: %d\n" !cycle_count
;;

let test_input = [
  "987654321111111";
  "111111111111111"
]

let%expect_test "test small numbers" =
    (* Construct the simulation and get its input and output ports. *)
    testbench test_input true;
    [%expect {|
      count=0
      count=33
      count=132
      count=1142
      count=1188513027
      count=1188735249
      count=1188735249
      count=1189181695
      count=1227775554
      count=1227775554
      count=1227775554
      count=1227775554
      Total cycles: 575
      |}]