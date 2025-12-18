open Hardcaml
open Hardcaml.Signal


module I = struct
  type 'a t =
    { clock    : 'a
    ; clear    : 'a
    ; finished : 'a
    ; valid    : 'a
    ; input    : 'a[@bits 8]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a
    ; count : 'a[@bits 32]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t = 
    | Inputting
    | Computing
    | Finished
  [@@deriving sexp_of, compare ~localize, enumerate]
end

(* Ram format:
  Bit 0: valid
  Bit 1: non-empty
  Bit 2: is operation
  Bit 3: 0 = +, 1 = *
  Bits 4-7: decimal number (4 bits) *)
let create w h (i : _ I.t) =
    let n = w * h in
    let address_width = Base.Int.ceil_log2 n in
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let ram_we    = Always.Variable.reg ~width:1 r_sync in
    let counter   = Always.Variable.reg ~width:address_width r_sync in
    let ram_waddr = Always.Variable.reg ~width:address_width r_sync in
    let ram_wdata = Always.Variable.reg ~width:8 r_sync in
    let ram_raddr_1 = Always.Variable.wire ~default:(of_int ~width:address_width 0) in
    let ram_raddr_2 = Always.Variable.wire ~default:(of_int ~width:address_width 0) in
    (* Instantiate the RAM *)
    let ram_out = 
      Ram.create 
        ~collision_mode:Read_before_write
        ~size:n
        ~write_ports:[| { write_clock = i.clock; write_enable = ram_we.value; write_address = ram_waddr.value; write_data = ram_wdata.value } |]
        ~read_ports:([| { read_clock = i.clock; read_enable = vdd; read_address = ram_raddr_1.value }
                      ; { read_clock = i.clock; read_enable = vdd; read_address = ram_raddr_2.value } |])
        ()
    in
    let ram_rdata_1 = ram_out.(0) in
    let _ram_rdata_2 = ram_out.(1) in

    let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in
    Always.(
      compile [
        sm.switch [
          (Inputting, [
            ram_we <-- i.valid
          ; ram_waddr <-- counter.value
          ; ram_wdata <-- concat_msb [
              i.valid; i.input <>:. (Char.code ' ')
            ; (i.input ==:. (Char.code '*')) |: (i.input ==:. (Char.code '*'))
            ; i.input ==:. (Char.code '+')
            ; uresize (i.input -:. (Char.code '0')) 4
            ]
          ; when_ i.valid [
              counter <-- counter.value +:. 1
            ]
          ; when_ i.finished [
              sm.set_next Computing
            ]
          ]);
          (Computing, [
            (* TODO: computation *)
            ram_we <-- gnd;
            ram_raddr_1 <-- of_int ~width:address_width 2;
            ram_raddr_2 <-- of_int ~width:address_width 2;
            sm.set_next Finished
          ]);
          (Finished, [
            ram_we <-- gnd
          ])
        ]
      ]
    );
    {O.ready = sm.is Finished;
     count = uresize ram_rdata_1 32
    }
;;

module Simulator = Cyclesim.With_interface(I)(O)

let testbench input _verbose =
  let cycle_count = ref 0 in
  let sim = Simulator.create (create (String.length (List.hd input)) (List.length input)) in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in

  inputs.clear := Bits.vdd;
  inputs.finished := Bits.gnd;
  inputs.valid := Bits.gnd;
  inputs.input := Bits.of_int ~width:8 0;
  cycle_count := !cycle_count + 1;
  Cyclesim.cycle sim;

  let stream ~line =
    String.iter (fun c ->
      inputs.clear := Bits.gnd;
      inputs.finished := Bits.gnd;
      inputs.valid := Bits.vdd;
      inputs.input := Bits.of_int ~width:8 (Char.code c);
      cycle_count := !cycle_count + 1;
      Cyclesim.cycle sim;
    ) line;
    if _verbose then
      Stdio.printf "part1_count=%s\n" (Bits.to_bstr (Bits.select !(outputs.count) 7 0))
  in
  List.iter (fun line -> stream ~line:line) input;
  (* indicate end of input *)
  inputs.clear := Bits.gnd;
  inputs.finished := Bits.vdd;
  inputs.valid := Bits.gnd;
  inputs.input := Bits.of_int ~width:8 0;
  cycle_count := !cycle_count + 1;
  Cyclesim.cycle sim;
  while (not (Bits.to_bool !(outputs.ready))) do
      inputs.clear := Bits.gnd;
      inputs.finished := Bits.gnd;
      inputs.valid := Bits.gnd;
      inputs.input := Bits.of_int ~width:8 0;
      cycle_count := !cycle_count + 1;
      Cyclesim.cycle sim;
    done;
  Stdio.printf "part1_count=%d\n" (Bits.to_int !(outputs.count));
  Stdio.printf "Total cycles: %d\n" !cycle_count
;;

let test_input = [
  "123 328  51 64 ";
  " 45 64  387 23 ";
  "  6 98  215 314";
  "*   +   *   +  ";  
]

let%expect_test "test circuit" =
    (* Construct the simulation and get its input and output ports. *)
    testbench test_input true;
    [%expect {|
      part1_count=11000001
      part1_count=11000001
      part1_count=11000001
      part1_count=11000001
      part1_count=195
      Total cycles: 63
      |}]