open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; lower : 'a[@bits 64]
    ; upper : 'a[@bits 64]
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a (* whether ready to receive input *)
    ; part1_count : 'a[@bits 64]
    ; part2_count : 'a[@bits 64]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t = 
    | ReadyForInput
    | ExtractingDigits1
    | ExtractingDigits2
    | CheckingEquality
    | Incrementing
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let add_and_shift dcb_value =
  let map_chunks f signal =
    let chunks = 
      List.init 16 (fun i ->
        select signal (i * 4 + 3) (i * 4)  
      ) 
    in
    let mapped_chunks = List.map f chunks in
    concat_lsb mapped_chunks
  in
  let added_result =
    map_chunks (fun chunk ->
      mux2 (chunk >:. 4) (chunk +:. 3) chunk 
    ) dcb_value
  in
  let shifted_result = sll added_result 1 in
  shifted_result
;;

let dcb_length s = srl ((of_int ~width:7 64) -: (select (leading_zeros s) 6 0) +:. 3) 2

let unique_factors i =
  let rec aux x n =
    if x == 0 then []
    else if x == 1 then [1]
    else if n mod x == 0 then x :: aux (x - 1) n
    else aux (x - 1) n
  in
  aux (i - 1) i
;;

let test_length s len =
  let factors = unique_factors len in
  List.map (fun factor ->
    let target_chunk = select s (factor * 4 - 1) 0 in (* all must match first chunk *)
    let num_chunks = len / factor in
    let chunks = 
      List.init num_chunks (fun i ->
        select s (i * factor * 4 + (factor * 4 - 1)) (i * factor * 4)
      )
    in
    List.fold_left (fun acc chunk ->
      acc &: (chunk ==: target_chunk)
    ) vdd chunks
  ) factors
  |> List.fold_left (fun acc factor -> acc |: factor) gnd
;;

let equality_by_length s = 
  List.init 16 (fun i ->
    let len = i + 1 in
    test_length s len
  )
;;

let part1_equality_by_length s = 
  List.init 8 (fun len ->
    let first_half = select s (len * 4 + 3) 0 in
    let second_half = select s (len * 8 + 7) (len * 4 + 4) in
    first_half ==: second_half
  )
;;

let dcb_ripple_incr s =
    let chunks = 
      List.init 16 (fun i ->
        select s (i * 4 + 3) (i * 4)  
      ) 
    in
    List.fold_left (fun (carry, acc) chunk ->
      let sum = chunk +: carry in
      let new_chunk = mux2 (sum >:. 9) (of_int ~width:4 0) sum in
      let new_carry = mux2 (sum >:. 9) (of_int ~width:4 1) (of_int ~width:4 0) in
      (new_carry, acc @ [new_chunk])
    ) (of_int ~width:4 1, []) chunks
    |> snd |> concat_lsb
;;

(* first extract the base 10 digits into digit_regfile with double dabble *)
(* check for equality with length check + concat + xor *)
(* increment efficiently by scanning *)
(* states are ReadyForInput -> ExtractingDigits (2 parts) -> CheckingEquality -> Incrementing *)
let create (i : _ I.t) =
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in
    let curr_reg = Always.Variable.reg ~width:64 ~enable:vdd r_sync in
    let upper_reg = Always.Variable.reg ~width:64 ~enable:vdd r_sync in
    let dcb_right = Always.Variable.reg ~width:64 ~enable:vdd r_sync in
    let dcb_regfile = Always.Variable.reg ~width:(16 * 4) ~enable:vdd r_sync in
    let dcb_timer = Always.Variable.reg ~width:8 ~enable:vdd r_sync in
    let part1_counter_reg = Always.Variable.reg ~width:64 ~enable:vdd r_sync in
    let part2_counter_reg = Always.Variable.reg ~width:64 ~enable:vdd r_sync in

    Always.(
      compile [
        sm.switch [
          (ReadyForInput, 
          [ curr_reg <-- i.lower
          ; upper_reg <-- i.upper
          ; when_ i.valid [
            sm.set_next ExtractingDigits1
          ]
          ]);
          (ExtractingDigits1,
          [ dcb_right <-- curr_reg.value
          ; dcb_regfile <--. 0
          ; dcb_timer <--. 0
          ; sm.set_next ExtractingDigits2
          ]
          );
          (ExtractingDigits2,
          let shift_added_dcb = add_and_shift dcb_regfile.value in
          let new_dcb = concat_msb [ select shift_added_dcb 63 1 ; msb dcb_right.value ] in
          [ dcb_regfile <-- new_dcb
          ; dcb_right <-- sll dcb_right.value 1
          ; dcb_timer <-- dcb_timer.value +:. 1
          ; when_ (dcb_timer.value ==:. 63) [
              sm.set_next CheckingEquality
            ]
          ]
          );
          (CheckingEquality,
          let by_length = equality_by_length dcb_regfile.value in
          let part1_by_length = part1_equality_by_length dcb_regfile.value in
          let length = dcb_length dcb_regfile.value in
          let half_length = srl length 1 in
          let is_even_length = (lsb length) ==:. 0 in
          [ when_ (mux (length -:. 1) by_length) [
            part2_counter_reg <-- part2_counter_reg.value +: curr_reg.value;
          ]
          ; when_ is_even_length [
            when_ (mux (half_length -:. 1) part1_by_length) [
              part1_counter_reg <-- part1_counter_reg.value +: curr_reg.value;
            ]
          ]
          ; if_ (curr_reg.value >=: upper_reg.value) [
            sm.set_next ReadyForInput
          ] [
            sm.set_next Incrementing
          ]
          ]);
          (Incrementing,
          [
            curr_reg <-- curr_reg.value +:. 1
          ; dcb_regfile <-- dcb_ripple_incr dcb_regfile.value
          ; sm.set_next CheckingEquality
          ]
          )
    ]]);
    { O.part1_count = part1_counter_reg.value
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
  inputs.lower := Bits.of_int ~width:64 0;
  inputs.upper := Bits.of_int ~width:64 0;
  inputs.clear := Bits.vdd;
  inputs.valid := Bits.gnd;
  Cyclesim.cycle sim;

  let step ~lower ~upper =
    (* wait for input to be available *)
    while not (Bits.to_bool !(outputs.ready)) do
      inputs.lower := Bits.of_int ~width:64 0;
      inputs.upper := Bits.of_int ~width:64 0;
      inputs.clear := Bits.gnd;
      inputs.valid := Bits.gnd;
      cycle_count := !cycle_count + 1;
      Cyclesim.cycle sim;
    done;
    (* provide input *)
    inputs.lower := Bits.of_int ~width:64 lower;
    inputs.upper := Bits.of_int ~width:64 upper;
    inputs.clear := Bits.gnd;
    inputs.valid := Bits.vdd;
    cycle_count := !cycle_count + 1;
    Cyclesim.cycle sim;
    if verbose then
      Stdio.printf "part1_count=%d, part2_count=%d\n" (Bits.to_int !(outputs.part1_count)) (Bits.to_int !(outputs.part2_count));
  in
  List.iter (fun (lower, upper) -> step ~lower ~upper) input;
  (* allow processing of final element *)
  while not (Bits.to_bool !(outputs.ready)) do
    inputs.lower := Bits.of_int ~width:64 0;
    inputs.upper := Bits.of_int ~width:64 0;
    inputs.clear := Bits.gnd;
    inputs.valid := Bits.gnd;
    cycle_count := !cycle_count + 1;
    Cyclesim.cycle sim;
  done;
  Stdio.printf "part1_count=%d, part2_count=%d\n" (Bits.to_int !(outputs.part1_count)) (Bits.to_int !(outputs.part2_count));
  Stdio.printf "Total cycles: %d\n" !cycle_count
;;

let test_input = [
  (11, 22);
  (95, 115);
  (998, 1012);
  (1188511880, 1188511890);
  (222220, 222224);
  (1698522, 1698528);
  (446443, 446449);
  (38593856, 38593862);
  (565653, 565659);
  (824824821, 824824827);
  (2121212118, 2121212124);
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

let%expect_test "equality_by_length test" =
  let s = of_int ~width:64 0b0000_0000_0000_0000_0000_0001_0001_0001_0000_0001_0001_0001_0000_0001_0001_0001 in
  let results = equality_by_length s in
  List.iteri (fun i res ->
    Stdio.printf "Length %d equality: %b\n" (i + 1) (Signal.to_bool res)
  ) results;
  [%expect {|
    Length 1 equality: false
    Length 2 equality: true
    Length 3 equality: true
    Length 4 equality: false
    Length 5 equality: false
    Length 6 equality: false
    Length 7 equality: false
    Length 8 equality: true
    Length 9 equality: false
    Length 10 equality: false
    Length 11 equality: false 
    Length 12 equality: true
    Length 13 equality: false
    Length 14 equality: false
    Length 15 equality: false
    Length 16 equality: false
    |}]

let%expect_test "dcb_ripple_increment test" = 
  let s = of_int ~width:64 0b0000_0000_0000_0000_0000_0000_0000_0000_0001_0010_0011_0100_0101_0110_0111_1001 in
  let incremented = dcb_ripple_incr s in
  Stdio.printf "Before: %s\n" (to_bstr s);
  Stdio.printf "After : %s\n" (to_bstr incremented);
  [%expect {|
    Before: 0000000000000000000000000000000000010010001101000101011001111001
    After : 0000000000000000000000000000000000010010001101000101011010000000
    |}]