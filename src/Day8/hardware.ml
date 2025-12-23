open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; finished : 'a
    ; x : 'a[@bits 16]
    ; y : 'a[@bits 16]
    ; z : 'a[@bits 16]
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

module RamIO = struct
  type 'a t =
  {
    ram_we    : 'a;
    ram_waddr : 'a;
    ram_wdata : 'a;
    ram_raddr : 'a;
    ram_rdata : 'a;
  }
end

let create_ram n bits (i : _ I.t) : _ RamIO.t =
  let address_width = Base.Int.ceil_log2 n in
  let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let ram_we    = Always.Variable.reg ~width:1 r_sync in
  let ram_waddr = Always.Variable.reg ~width:address_width r_sync in
  let ram_wdata = Always.Variable.reg ~width:bits r_sync in
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
  { RamIO.ram_we    = ram_we.value
  ; RamIO.ram_waddr = ram_waddr.value
  ; RamIO.ram_wdata = ram_wdata.value
  ; RamIO.ram_raddr = ram_raddr.value
  ; RamIO.ram_rdata = ram_rdata
  }
;;

(* idea: only solve part 1 *)
(* keep memory of size 5 * width *)
(* circular array + update as you go *)
(* only need 3 bits per cell (1 for if @, 4 for nbr count *)
(* n is width of the array *)
let create n (i : _ I.t) =
    let _values_ram = create_ram n 48 i in
    let _in_mst_ram = create_ram n 1 i in
    let _parent_ram = create_ram n 16 i in
    let _key_ram = create_ram n 64 i in
    { O.ready = vdd
    ; O.count = of_int ~width:32 0
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
  inputs.x := Bits.of_int ~width:16 0;
  inputs.y := Bits.of_int ~width:16 0;
  inputs.z := Bits.of_int ~width:16 0;
  cycle_count := !cycle_count + 1;
  Cyclesim.cycle sim;

  let cycle ~point =
    inputs.clear := Bits.gnd;
    inputs.finished := Bits.gnd;
    inputs.x := Bits.of_int ~width:16 (fst3 point);
    inputs.y := Bits.of_int ~width:16 (snd3 point);
    inputs.z := Bits.of_int ~width:16 (trd3 point);
    cycle_count := !cycle_count + 1;
    Cyclesim.cycle sim;
  in
  List.iter (fun point -> cycle ~point:point) input;
  (* indicate end of input *)
  inputs.clear := Bits.gnd;
  inputs.finished := Bits.vdd;
  cycle_count := !cycle_count + 1;
  Cyclesim.cycle sim;
  while (not (Bits.to_bool !(outputs.ready))) do
      inputs.clear := Bits.gnd;
      inputs.finished := Bits.gnd;
      inputs.x := Bits.of_int ~width:16 0;
      inputs.y := Bits.of_int ~width:16 0;
      inputs.z := Bits.of_int ~width:16 0;
      cycle_count := !cycle_count + 1;
      Cyclesim.cycle sim;
    done;
  Stdio.printf "part1_count=%d\n" (Bits.to_int !(outputs.count));
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