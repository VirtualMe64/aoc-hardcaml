open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; new_input : 'a
    ; number : 'a[@bits 4]
    ; remaining : 'a[@bits 8] (* number of characters remaining after current *)
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
    { O.value = value_reg
    ; O.valid = valid_reg
    ; O.number = number_reg
    ; O.min_index = min_index_reg
    ; O.zeroing = zeroing_reg
    }
  ;;
end

let bcd_file_to_int_signal values n =
  let rec pow_of_10 n =
    if n = 0 then 1 else 10 * pow_of_10 (n - 1)
  in
  let segments = List.mapi (fun i bcd ->
    uresize (bcd *: of_int ~width:(n * 4) (pow_of_10 i)) (n * 4)
  ) values
  in
  List.fold_left (+:) (of_int ~width:(n * 4) 0) segments
;;

let construct_processor_file n ~clock ~clear ~new_input ~valid ~number ~remaining =
  List.fold_left (fun acc idx ->
  let proc_i = Processor.create {
    index = of_int ~width:4 idx;
    clock;
    clear = clear |: new_input;
    valid = if idx == 0 then valid else (List.hd acc).Processor.O.valid;
    number = if idx == 0 then number else (List.hd acc).Processor.O.number;
    min_index = if idx == 0 then mux2
        (remaining >:. 11) (of_int ~width:4 0) ((of_int ~width:4 11) -: select remaining 3 0)
      else
        (List.hd acc).Processor.O.min_index;
    zeroing = if idx == 0 then gnd else (List.hd acc).Processor.O.zeroing;
  } in proc_i :: acc
  ) [] (List.init n (fun i -> i))
;;

(* streaming processor *)
(* inputs passed between *)
let create (i : _ I.t) =
    let processor_file = construct_processor_file 12
      ~clock:i.clock
      ~clear:i.clear
      ~new_input:i.new_input
      ~valid:i.valid
      ~number:i.number
      ~remaining:i.remaining
    in
    let ready_signal = ~: (List.fold_left (fun acc p -> acc |: p.Processor.O.valid) gnd processor_file) in
    let result_signal = bcd_file_to_int_signal (List.map (fun p -> p.Processor.O.value) processor_file) 12 in
    let ready_reg = reg_fb (Reg_spec.create ~clock:i.clock ~clear:i.clear () ) ~enable:vdd ~width:1
      ~f:(fun _ -> ready_signal)
    in
    let rising_edge = ready_signal &: ~:ready_reg in
    let count_reg = reg_fb (Reg_spec.create ~clock:i.clock ~clear:i.clear () ) ~enable:vdd ~width:64
      ~f:(fun v -> mux2
        rising_edge
        (v +: uresize result_signal 64)
        v
      )
    in
    { O.ready = ready_reg
    ; O.count = count_reg
    }
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
      Stdio.printf "count=%d\n" (Bits.to_int !(outputs.count));
  in
  List.iter (fun line -> stream ~number:line) input;
  Stdio.printf "part2_count=%d\n" (Bits.to_int !(outputs.count));
  Stdio.printf "Total cycles: %d\n" !cycle_count
;;

let test_input = [
  "987654321111111";
  "811111111111119";
  "234234234234278";
  "818181911112111"
]

let%expect_test "test small numbers" =
    (* Construct the simulation and get its input and output ports. *)
    testbench test_input true;
    [%expect {|
      count=987654321111
      count=1798765432230
      count=2232999666508
      count=3121910778619
      part2_count=3121910778619
      Total cycles: 116
      |}]