open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; input : 'a[@bits 8]
    ; valid : 'a
    ; finished : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a (* whether output is ready *)
    ; count : 'a[@bits 32]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t = 
    | Idle
    | Streaming
    | Flushing
  [@@deriving sexp_of, compare ~localize, enumerate]
end

(* idea: only solve part 1 *)
(* keep memory of size 5 * width *)
(* circular array + update as you go *)
(* only need 3 bits per cell (1 for if @, 4 for nbr count *)
(* n is width of the array *)
let create n (i : _ I.t) =
    let address_width = Base.Int.ceil_log2 n in
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let ram_we    = Always.Variable.reg ~width:1 r_sync in
    let ram_waddr = Always.Variable.reg ~width:address_width r_sync in
    let ram_wdata = Always.Variable.reg ~width:5 r_sync in
    let ram_raddr = Always.Variable.wire ~default:(of_int ~width:address_width 0) in
    (* Instantiate the RAM *)
    let ram_out = 
      Ram.create 
        ~collision_mode:Read_before_write
        ~size:n
        ~write_ports:[| { write_clock = i.clock; write_enable = ram_we.value; write_address = ram_waddr.value; write_data = ram_wdata.value } |]
        ~read_ports:[| { read_clock = i.clock; read_enable = vdd; read_address = ram_raddr.value } |]
        ()
    in
    let ram_rdata = ram_out.(0) in
    let timer = Always.Variable.reg ~width:(address_width + 2) r_sync in
    let curr_addr = Always.Variable.reg ~width:address_width r_sync in
    let prev_buffer1 = Always.Variable.reg ~width:5 r_sync in
    let prev_buffer2 = Always.Variable.reg ~width:5 r_sync in
    let prev_buffer3 = Always.Variable.reg ~width:5 r_sync in
    let left_buffer = Always.Variable.reg ~width:5 r_sync in
    let count = Always.Variable.reg ~width:32 r_sync in

    let occupied = (i.valid) &: (i.input ==:. (Char.code '@')) in

    let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in
    
    (* extract streaming logic so it can be applied on first cycle *)
    let streaming_logic =
      (* construct value for curr idx (first bit for occupied, rest for neighbors) *)
      let new_left_buffer = concat_msb
        [ occupied;
          (mux2 (curr_addr.value ==:. n - 1) 
            (of_int ~width:4 0)
            (uresize (msb ram_rdata) 4)) +:
          (uresize (msb prev_buffer3.value) 4) +:
          (mux2 (curr_addr.value ==:. 0)
            (of_int ~width:4 0)
            ((uresize (msb prev_buffer2.value) 4) +:
            (uresize (msb left_buffer.value) 4)))
        ] in

      (* if this is occupied, add 1 to previous neighbors *)
      let new_buffer1 = mux2 (occupied &: (curr_addr.value >:. 0)) (prev_buffer2.value +: (of_int ~width:5 1)) prev_buffer2.value in
      let new_buffer2 = mux2 occupied (prev_buffer3.value +: (of_int ~width:5 1)) prev_buffer3.value in
      let new_buffer3 = mux2 (occupied &: (curr_addr.value <:. (n - 1))) (ram_rdata +: (of_int ~width:5 1)) ram_rdata in
      let writeback = mux2 (occupied &: (curr_addr.value >:. 0))
        (left_buffer.value +: (of_int ~width:5 1)) left_buffer.value in

      Always.[
        curr_addr <-- mux2 (curr_addr.value ==:. n - 1) (of_int ~width:address_width 0) (curr_addr.value +:. 1)
      ; when_ ((select prev_buffer1.value 3 0 <:. 4) &: (msb prev_buffer1.value ==:. 1)) [
          count <-- count.value +:. 1
        ]

      ; left_buffer <-- new_left_buffer
      ; prev_buffer1 <-- new_buffer1
      ; prev_buffer2 <-- new_buffer2
      ; prev_buffer3 <-- new_buffer3

      ; ram_we <-- vdd
      ; ram_waddr <-- mux2 (curr_addr.value ==:. 0) (of_int ~width:address_width (n - 1)) (curr_addr.value -:. 1)
      ; ram_wdata <-- writeback
      ; ram_raddr <-- mux2 (curr_addr.value >=:. n - 2) (curr_addr.value -:. n +:. 2) (curr_addr.value +:. 2)
      ]
    in
    Always.(
      compile [
        sm.switch [
          (Idle, 
          [ when_ i.valid (
              [sm.set_next Streaming] @ streaming_logic
            )
          ]);
          (Streaming, [
            when_ i.finished [
              timer <-- of_int ~width:(address_width + 2) 0
            ; sm.set_next Flushing
            ]
          ] @ streaming_logic);
          (Flushing,
          [ timer <-- timer.value +:. 1
          ; when_ (timer.value ==: of_int ~width:(address_width + 2) (n) ) [
              sm.set_next Idle
          ]
          ] @ streaming_logic)
        ]
      ]
    );
    { O.ready = sm.is Idle
    ; O.count = count.value
    }
;;

module Simulator = Cyclesim.With_interface(I)(O)

let testbench input _verbose =
  let cycle_count = ref 0 in
  let sim = Simulator.create (create (String.length (List.hd input))) in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in

  inputs.clear := Bits.vdd;
  inputs.valid := Bits.gnd;
  inputs.input := Bits.of_int ~width:8 0;
  cycle_count := !cycle_count + 1;
  inputs.finished := Bits.gnd;
  Cyclesim.cycle sim;

  let stream ~line =
    String.iter (fun c ->
      inputs.clear := Bits.gnd;
      inputs.valid := Bits.vdd;
      inputs.input := Bits.of_int ~width:8 (Char.code c);
      inputs.finished := Bits.gnd;
      cycle_count := !cycle_count + 1;
      Cyclesim.cycle sim;
    ) line;
    if _verbose then
      Stdio.printf "part1_count=%d\n" (Bits.to_int !(outputs.count))
  in
  List.iter (fun line -> stream ~line:line) input;
  (* indicate end of input *)
  inputs.clear := Bits.gnd;
  inputs.valid := Bits.gnd;
  inputs.input := Bits.of_int ~width:8 0;
  inputs.finished := Bits.vdd;
  cycle_count := !cycle_count + 1;
  Cyclesim.cycle sim;
  while (not (Bits.to_bool !(outputs.ready))) do
      inputs.clear := Bits.gnd;
      inputs.valid := Bits.gnd;
      inputs.input := Bits.of_int ~width:8 0;
      inputs.finished := Bits.gnd;
      cycle_count := !cycle_count + 1;
      Cyclesim.cycle sim;
    done;
  Stdio.printf "part1_count=%d\n" (Bits.to_int !(outputs.count));
  Stdio.printf "Total cycles: %d\n" !cycle_count
;;

let test_input = [
  "..@@.@@@@.";
  "@@@.@.@.@@";
  "@@@@@.@.@@";
  "@.@@@@..@.";
  "@@.@@@@.@@";
  ".@@@@@@@.@";
  ".@.@.@.@@@";
  "@.@@@.@@@@";
  ".@@@@@@@@.";
  "@.@.@@@.@.";
]

(* let test_input = [
  "0000000001";
] *)

let%expect_test "test circuit" =
    (* Construct the simulation and get its input and output ports. *)
    testbench test_input true;
    [%expect {|
      debug1: 2, debug2: 1, count=0
      debug1: 19, debug2: 19, count=4
      debug1: 21, debug2: 21, count=6
      debug1: 20, debug2: 5, count=7
      debug1: 20, debug2: 19, count=7
      debug1: 20, debug2: 19, count=8
      debug1: 7, debug2: 18, count=9
      debug1: 23, debug2: 3, count=9
      debug1: 23, debug2: 19, count=10
      debug1: 21, debug2: 4, count=10
      debug1: 18, debug2: 2, count=12
      debug1: 1, debug2: 0, count=13
      Total cycles: 121
      |}]