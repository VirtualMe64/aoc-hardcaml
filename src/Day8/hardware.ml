open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; finished : 'a
    ; valid: 'a
    ; x : 'a[@bits 32]
    ; y : 'a[@bits 32]
    ; z : 'a[@bits 32]
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
    | Inputting
    | PrimsOuterLoop
    | PrimsLoop1
    | PrimsLoop2
    | Finished
  [@@deriving sexp_of, compare ~localize, enumerate]
end

module RamAccess = struct
  type t =
  {
    ram_we    : Always.Variable.t;
    ram_waddr : Always.Variable.t;
    ram_wdata : Always.Variable.t;
    ram_raddr : Always.Variable.t;
    ram_rdata : Signal.t;
  }
end

let create_ram n bits (i : _ I.t) : RamAccess.t =
  let address_width = Base.Int.ceil_log2 n in
  let ram_we    = Always.Variable.wire ~default:(of_int ~width:1 0) in
  let ram_waddr = Always.Variable.wire ~default:(of_int ~width:address_width 0) in
  let ram_wdata = Always.Variable.wire ~default:(of_int ~width:bits 0) in
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
  { RamAccess.ram_we    = ram_we
  ; RamAccess.ram_waddr = ram_waddr
  ; RamAccess.ram_wdata = ram_wdata
  ; RamAccess.ram_raddr = ram_raddr
  ; RamAccess.ram_rdata = ram_rdata
  }
;;

(* idea: only solve part 1 *)
(* keep memory of size 5 * width *)
(* circular array + update as you go *)
(* only need 3 bits per cell (1 for if @, 4 for nbr count *)
(* n is width of the array *)
let create n (i : _ I.t) =
  let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let address_width = Base.Int.ceil_log2 n in
  let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in
  let counter = Always.Variable.reg ~width:32 ~enable:vdd r_sync in
  let outer_loop_index = Always.Variable.reg ~width:32 ~enable:vdd r_sync in
  let inner_loop_index = Always.Variable.reg ~width:32 ~enable:vdd r_sync in
  let min_idx = Always.Variable.reg ~width:32 ~enable:vdd r_sync in
  let min_key = Always.Variable.reg ~width:64 ~enable:vdd r_sync in
  let min_idx_value = Always.Variable.reg ~width:96 ~enable:vdd r_sync in

  let values_ram = create_ram n 96 i in
  let in_mst_ram = create_ram n 1 i in
  let key_ram = create_ram n 64 i in (* first bit valid, next 63 for key *)
  let parent_ram = create_ram n 16 i in (* first bit valid, next 15 for value *)

  let norm value1 value2 =
    let abs_diff a b =
      mux2 (a <: b) (b -: a) (a -: b)
    in
    let x1 = select value1 31 0 in
    let x2 = select value2 31 0 in
    let y1 = select value1 63 32 in
    let y2 = select value2 63 32 in
    let z1 = select value1 95 64 in
    let z2 = select value2 95 64 in
    let dx = uresize (abs_diff x1 x2) 32 in
    let dy = uresize (abs_diff y1 y2) 32 in
    let dz = uresize (abs_diff z1 z2) 32 in
    let sum = 
      (dx *: dx) +:
      (dy *: dy) +:
      (dz *: dz) in
    concat_msb [ of_int ~width:1 1 ; select sum 62 0]
  in

  Always.(
    compile [
      sm.switch [
        (Idle, [
          when_ (i.valid) [
            sm.set_next Inputting
            ; values_ram.ram_we <-- vdd
            ; values_ram.ram_waddr <-- uresize counter.value address_width
            ; values_ram.ram_wdata <-- concat_msb [ i.x; i.y; i.z ]
            ; counter <-- counter.value +: of_int ~width:32 1
          ]
        ; key_ram.ram_we <--. 1
        ; key_ram.ram_waddr <--. 0
        ; key_ram.ram_wdata <-- concat_msb [ of_int ~width:1 1; of_int ~width:63 0 ]
        ]);
        (Inputting, [ 
          when_ (i.finished) [
            sm.set_next PrimsOuterLoop
          ]
        ; values_ram.ram_we <-- vdd
        ; values_ram.ram_waddr <-- uresize counter.value address_width
        ; values_ram.ram_wdata <-- concat_msb [ i.x; i.y; i.z ]
        ; counter <-- counter.value +: of_int ~width:32 1
        ]);
        (PrimsOuterLoop, [
          if_ (outer_loop_index.value ==:. n) [
            sm.set_next Finished
          ] [  
            outer_loop_index <-- outer_loop_index.value +: of_int ~width:32 1
          ; min_idx <-- of_int ~width:32 0
          ; min_key <-- of_int ~width:64 0
          ; inner_loop_index <--. 0
          ; in_mst_ram.ram_raddr <--. 0
          ; key_ram.ram_raddr <--. 0
          ; sm.set_next PrimsLoop1
          ]
        ]);
        (PrimsLoop1, [
          if_ (inner_loop_index.value ==:. n) [
            sm.set_next PrimsLoop2
          ; min_idx_value <-- values_ram.ram_rdata
          ; in_mst_ram.ram_we <--. 1
          ; in_mst_ram.ram_waddr <-- uresize min_idx.value address_width
          ; in_mst_ram.ram_wdata <--. 1
          ; in_mst_ram.ram_raddr <--. 0
          ; key_ram.ram_raddr <--. 0
          ; values_ram.ram_raddr <--. 0
          ; parent_ram.ram_raddr <--. 0
          ; inner_loop_index <--. 0
          ] [
            sm.set_next PrimsLoop1
          ; inner_loop_index <-- inner_loop_index.value +:. 1
          ; key_ram.ram_raddr <-- uresize (inner_loop_index.value +:. 1) address_width
          ; in_mst_ram.ram_raddr <-- uresize (inner_loop_index.value +:. 1) address_width
          ; when_ ((in_mst_ram.ram_rdata ==:. 0) &:
              ((key_ram.ram_rdata <: min_key.value) |: (select min_key.value 63 63 ==:. 0))) [
              min_key <-- key_ram.ram_rdata
            ; min_idx <-- inner_loop_index.value
            ]
          ; values_ram.ram_raddr <-- uresize min_idx.value address_width
          ]
        ]);
        (PrimsLoop2, [
          if_ (inner_loop_index.value ==:. n) [
            sm.set_next PrimsOuterLoop
          ] [
            sm.set_next PrimsLoop2
          ; inner_loop_index <-- inner_loop_index.value +:. 1
          ; key_ram.ram_raddr <-- uresize (inner_loop_index.value +:. 1) address_width
          ; values_ram.ram_raddr <-- uresize (inner_loop_index.value +:. 1) address_width
          ; in_mst_ram.ram_raddr <-- uresize (inner_loop_index.value +:. 1) address_width
          ; parent_ram.ram_raddr <-- uresize (inner_loop_index.value +:. 1) address_width
          ; when_ ((in_mst_ram.ram_rdata ==:. 0) &:
            (((norm values_ram.ram_rdata min_idx_value.value) <: key_ram.ram_rdata) |: (select key_ram.ram_rdata 63 63 ==:. 0))) [
              key_ram.ram_we <--. 1
            ; key_ram.ram_waddr <-- uresize inner_loop_index.value address_width
            ; key_ram.ram_wdata <-- norm values_ram.ram_rdata min_idx_value.value
            ; parent_ram.ram_we <--. 1
            ; parent_ram.ram_waddr <-- uresize inner_loop_index.value address_width
            ; parent_ram.ram_wdata <-- uresize min_idx.value 16 
            ]
          ]
        ]);
        (Finished, [
          key_ram.ram_raddr <--. 1
        ]);
      ]
    ]
  );
  { O.ready = sm.is Finished
  ; O.count = uresize key_ram.ram_rdata 32
  }
;;

module Simulator = Cyclesim.With_interface(I)(O)

let testbench input _verbose =
  let fst3 (x,_,_) = x in
  let snd3 (_,y,_) = y in
  let trd3 (_,_,z) = z in
  let cycle_count = ref 0 in
  let sim = Simulator.create (create (List.length input)) in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in

  inputs.clear := Bits.vdd;
  inputs.finished := Bits.gnd;
  inputs.valid := Bits.gnd;
  cycle_count := !cycle_count + 1;
  Cyclesim.cycle sim;

  let cycle ~point =
    inputs.clear := Bits.gnd;
    inputs.finished := Bits.gnd;
    inputs.valid := Bits.vdd;
    inputs.x := Bits.of_int ~width:32 (fst3 point);
    inputs.y := Bits.of_int ~width:32 (snd3 point);
    inputs.z := Bits.of_int ~width:32 (trd3 point);
    cycle_count := !cycle_count + 1;
    Cyclesim.cycle sim;
  in
  List.iter (fun point -> cycle ~point:point) input;
  (* indicate end of input *)
  inputs.clear := Bits.gnd;
  inputs.finished := Bits.vdd;
  inputs.valid := Bits.gnd;
  cycle_count := !cycle_count + 1;
  Cyclesim.cycle sim;
  while (not (Bits.to_bool !(outputs.ready))) do
      inputs.clear := Bits.gnd;
      inputs.finished := Bits.gnd;
      inputs.valid := Bits.gnd;
      cycle_count := !cycle_count + 1;
      Cyclesim.cycle sim;
      if _verbose then
        Stdio.printf "part2_count=%d\n" (Bits.to_int !(outputs.count));
    done;
  Cyclesim.cycle sim;  
  Cyclesim.cycle sim;
  Stdio.printf "part2_count=%d\n" (Bits.to_int !(outputs.count));
  Stdio.printf "Total cycles: %d\n" !cycle_count
;;

let test_input = [
  (162,817,812);
  (57,618,57);
  (906,360,560);
  (592,479,940);
  (352,342,300);
  (466,668,158);
  (542,29,236);
  (431,825,988);
  (739,650,466);
  (52,470,668);
  (216,146,977);
  (819,987,18);
  (117,168,530);
  (805,96,715);
  (346,949,466);
  (970,615,88);
  (941,993,340);
  (862,61,35);
  (984,92,344);
  (425,690,689);
]

let%expect_test "test circuit" =
    (* Construct the simulation and get its input and output ports. *)
    testbench test_input true;
    [%expect {|
      part1_count=0
      part1_count=4
      part1_count=6
      part1_count=7
      part1_count=7
      part1_count=8
      part1_count=9
      part1_count=9
      part1_count=10
      part1_count=10
      part1_count=13
      Total cycles: 113
      |}]