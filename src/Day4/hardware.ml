open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; input : 'a
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a (* whether output is valid/ready to receive input *)
    ; debug1 : 'a[@bits 64]
    ; debug2 : 'a[@bits 64]
    ; count : 'a[@bits 64]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t = 
    | Idle
    | Streaming
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
    let curr_addr = Always.Variable.reg ~width:address_width r_sync in
    let prev_buffer1 = Always.Variable.reg ~width:5 r_sync in
    let prev_buffer2 = Always.Variable.reg ~width:5 r_sync in
    let prev_buffer3 = Always.Variable.reg ~width:5 r_sync in
    let left_buffer = Always.Variable.reg ~width:5 r_sync in
    let count = Always.Variable.reg ~width:64 r_sync in

    let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in
    
    (* extract streaming logic so it can be applied on first cycle *)
    let streaming_logic =
      let new_left_buffer = concat_msb
        [ i.input;
          (mux2 (curr_addr.value ==:. n - 1) 
            (of_int ~width:4 0)
            (uresize (msb ram_rdata) 4)) +:
          (uresize (msb prev_buffer3.value) 4) +:
          (mux2 (curr_addr.value ==:. 0)
            (of_int ~width:4 0)
            ((uresize (msb prev_buffer2.value) 4) +:
            (uresize (msb left_buffer.value) 4)))
        ] in
      let new_buffer1 = mux2 (i.input &: (curr_addr.value >:. 0)) (prev_buffer2.value +: (of_int ~width:5 1)) prev_buffer2.value in
      let new_buffer2 = mux2 i.input (prev_buffer3.value +: (of_int ~width:5 1)) prev_buffer3.value in
      let new_buffer3 = mux2 (i.input &: (curr_addr.value <:. (n - 1))) (ram_rdata +: (of_int ~width:5 1)) ram_rdata in
      let writeback = mux2 (i.input &: (curr_addr.value >:. 0))
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
          (Streaming, streaming_logic)
        ]
      ]
    );
    { O.ready = vdd
    ; O.debug1 = uresize prev_buffer1.value 64
    ; O.debug2 = uresize ram_rdata 64
    ; O.count = count.value
    }
;;

module Simulator = Cyclesim.With_interface(I)(O)

let testbench input _verbose =
  let cycle_count = ref 0 in
  let sim = Simulator.create (create 10) in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in

  inputs.clear := Bits.vdd;
  inputs.valid := Bits.gnd;
  inputs.input := Bits.gnd;
  cycle_count := !cycle_count + 1;
  Cyclesim.cycle sim;


  let stream ~line =
    String.iter (fun c ->
      let value = if c = '1' then Bits.vdd else Bits.gnd in
      inputs.clear := Bits.gnd;
      inputs.valid := Bits.vdd;
      inputs.input := value;
      cycle_count := !cycle_count + 1;
      Cyclesim.cycle sim;
    ) line;
    if _verbose then
      Stdio.printf "debug1: %d, debug2: %d, count=%d\n" (Bits.to_int !(outputs.debug1)) (Bits.to_int !(outputs.debug2)) (Bits.to_int !(outputs.count))
  in
  List.iter (fun line -> stream ~line:line) input;
      while (not (Bits.to_bool !(outputs.ready))) do
      inputs.input := Bits.gnd;
      inputs.clear := Bits.gnd;
      inputs.valid := Bits.gnd;
      cycle_count := !cycle_count + 1;
      Cyclesim.cycle sim;
    done;
  Stdio.printf "Total cycles: %d\n" !cycle_count
;;

let test_input = [
  "0011011110";
  "1110101011";
  "1111101011";
  "1011110010";
  "1101111011";
  "0111111101";
  "0101010111";
  "1011101111";
  "0111111110";
  "1010111010";
  "0000000000";
  "0000000000";
]

(* let test_input = [
  "1111111111";
  "0000000000";
  "0000000000";
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