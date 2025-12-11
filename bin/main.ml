open Hardcaml

module CounterCircuit = Circuit.With_interface(Counter.I)(Counter.O)
let circuit = CounterCircuit.create_exn Counter.create ~name:"counter"

let () = Rtl.print Verilog circuit