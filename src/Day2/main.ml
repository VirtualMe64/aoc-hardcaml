open Hardcaml
open Hardware_day2.Hardware

module Simulator = Cyclesim.With_interface(I)(O)