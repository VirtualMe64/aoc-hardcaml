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

I originally tried to solve this without a state machine, but without a mod operation I couldn't think of another way to handle numbers greater than 100, and switched to an approach where I can use multiple cycles per input (via ready and valid signals).

**Performance**: Executes in 12208 cycles (~2.93 per input line).

### Day 2

My Day 2 solution uses a state machine to process the input numbers:

*   **Waiting**: The circuit waits for input.
*   **Extracting Digits**: Uses the Double Dabble algorithm to extract decimal digits.
*   **Checking Equality**: Checks each possible chunk size to see if the digits are repeating.
*   **Incrementing**: Performs a ripple add operation.

Implement the equality check was the most interesting part of this circuit. For each length 1 through 16, I use a recursive function to find the factors. I then use two left fold operations to first check if all of the chunks have the same value for a given chunk size, then to see if any chunk size meets the requirement for each length. These would all be computed in parallel in hardware, then a mux with the number of digits is used to get the result.

**Performance**: Takes 4963923 cycles (~2.00 per number checked).

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