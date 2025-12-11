# Advent of FPGA 2025 - Hardcaml Solutions

This repository contains my solutions for [Jane Street's Advent of FPGA 2025](https://blog.janestreet.com/advent-of-fpga-challenge-2025/) challenge.

## About

I have never used OCaml or Hardcaml before, though I do have some experience with other HDLs. I am using this challenge as an opportunity to learn both OCaml/Hardcaml and to improve my architecture skills.

## Project Layout

The project is structured with each day's circuit in its own folder within `src/`.

*   `src/DayX/`: Contains the solution for Day X.
    *   `hardware.ml`: The actual hardware implementation of the circuit. Also includes tests for the circuit
    *   `main.ml`: Handles input parsing and runs the hardware simulation against the input.
*   `inputs/`: Contains the input files for the challenges.

## Solutions

### Day 1

My Day 1 solution implements a state machine with two states: `ReadyForInput` and `Processing`.

*   **ReadyForInput**: The circuit waits for a valid input signal. When received, it latches the `amount` and `direction` and transitions to `Processing`.
*   **Processing**: The circuit updates the rotation based on the direction.
    *   **Large Numbers**: To handle amounts larger than 100, the circuit subtracts 100 from the amount in each cycle until it is within range.
    *   **Counters**: I maintain counters for Part 1 and Part 2, incrementing them based on the rotation and overflow conditions.

## Usage

### Setup
Assuming you already have OCaml installed, create a switch with `opam switch create hardcaml 5.2.1` then run `opam install hardcaml ppx_hardcaml hardcaml_waveterm`

### Running Tests

You can run the tests for a specific day using `dune test`. For example, to test Day 1:

```bash
dune test src/Day1# aoc-hardcaml
```

### Running the solution
To run the solution against the input file, use `dune exec`. For example, for Day 1:
```bash
dune build
dune exec day1
```