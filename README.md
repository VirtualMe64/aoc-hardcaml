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

[Problem](https://adventofcode.com/2025/day/1) | [Solution](src/Day1/hardware.ml)

This was my first time making any non-trivial Hardcaml circuit, so I spent a lot of time getting the basics of file format, structure, and testing working. My initial approach only handled numbers in the range [0, 200], and took a single cycle for each input. To make it work with larger inputs, I switched to a state machine and repeatedly subtract 100 until the rotation is in the range [0, 100]. Short of implementing modular arithmetic, I'm happy with this solution.

![System diagram](images/day1.png)

**Performance**: Executes in 12208 cycles (~2.934 per line).

### Day 2

[Problem](https://adventofcode.com/2025/day/2) | [Solution](src/Day2/hardware.ml)

I used a multiphase approach for day two. After receiving the input, I extract the decimal digits using the Double Dabble algorithm, check for equality among all relevant sets of chunks, and then increment with a ripple add operation.

Implementing the equality check was the most interesting part of this circuit. For each length 1 through 16, I use a recursive function to find the factors. I then use two left fold operations to first check if all of the chunks have the same value for a given chunk size, then to see if any chunk size meets the requirement for each length. These are all computed in parallel, then a mux with the number of digits is used to get the result.

To solve part 1, I use the same logic but explicitly checking for a chunk size of `length // 2`

**Performance**: Executes in 4963923 cycles (~2.000 per number checked).

### Day 3

[Problem](https://adventofcode.com/2025/day/3) | [Solution](src/Day3/hardware.ml)

This is my favorite circuit so far! My reference python implementation iterated over each of the output digits for each input character, which would be somewhat inefficient and didn't feel in the spirit of an FPGA. Instead, I initialize a processor for each digit, and the characters are passed through them from the MSB to the LSB. This means with sufficiently long input strings we'd approach one cycle per character!

![System diagram](images/day3.png)

I'm also quite happy with how this approach generalized -- part 1 and part 2 use the same exact approach, part 1 with a chain of 2 processors and part 2 with a chain of 12.

**Performance**: Executes in 22800 cycles (114 per line, 1.14 per character)

### Day 4 (Part 1 Only)

[Problem](https://adventofcode.com/2025/day/4) | [Solution](src/Day4/hardware.ml)

Day 4 was the first problem where I only decided to solve one of the parts. I may come back to this with a brute force approach for part 2, since my optimized approach seems difficult to translate to an FPGA.

Since the first part only has dependencies of length `w` for a `w x h` grid, I was able to create a circuit which only uses `O(w)` memory and about `w * h` cycles. The input to the circuit is a left to right, top to bottom stream of characters straight from the file. The core idea is to allocate a size `w` ram which stores the state of the previous `w` cells, specifically 1 bit for if it's occupied and 4 bits for the known number of neighbors. Each new cell we read both updates its earlier neighbors and reads from them to determine its initial number of neighbors.

![System diagram](images/day4.png)


The circuit came together relatively quickly, but there were several hours of debugging to deal with off by one errors, register/ram timing issues, flushing, and handling boundary cases. I'm very happy with the result though, and I doubt it could be significantly improved for a 1-character-at-a-time streaming input.

**Performance** Executes in 19185 cycles (~1.007 per character)

### Day 5

[Problem](https://adventofcode.com/2025/day/5) | [Solution](src/Day5/hardware.ml)

My solution uses a chain of processing elements, each capable of storing one range. During the range input phase, each processor either stores a new range or passes it along to the next processor in the chain. When a range with a lower bound less than or equal to the stored range arrives, it evicts the stored range and takes its place — this eviction process ensures ranges end up both sorted and disjoint which makes the part 2 result easy to compute.

During the ID phase, each number passes through the chain. If a processor's stored range contains the number, it increments its match counter and doesn't forward the number. For part 2, we can sum the width of each valid processor -- there's no overlap since they're each disjoint!.

The amount of processing elements is specified in the config, but it must be at least the number of ranges in the input. This means the design is optimized for a large number of ids rather than ranges, but it is very low latency in those cases.

**Performance**: Executes in 1383 cycles for 1184 inputs (~n + max_ranges)

### Day 6

[Problem](https://adventofcode.com/2025/day/6) | [Solution](src/Day6/hardware.ml)

For Day 6, I implemented a store-then-process architecture. The circuit first streams the entire input grid into a dual-port RAM, pre-decoding the characters into flags (valid, is_op, numeric value) as they are stored. This simplifies the subsequent logic by removing the need for repeated ASCII decoding.

Once the input is fully loaded, two independent state machines run in parallel to solve Part 1 and Part 2 simultaneously. The RAM's two read ports allow both logic blocks to access the grid data without contention.
*   **Part 1** scans the grid to identify columns, accumulating BCD digits vertically and applying the operator found at the bottom.
*   **Part 2** traverses the grid to handle the "cephalopod math" rules, constructing numbers from vertical digits.

Both parts utilize 64-bit scratch registers to accumulate intermediate values and the grand totals. This approach proved to be very efficient, effectively pipelining the two parts and utilizing the memory bandwidth fully.

**Performance**: Executes in 37722 cycles (~2.01 per character).

### Day 7

[Problem](https://adventofcode.com/2025/day/7) | [Solution](src/Day7/hardware.ml)

My solution uses an array of processing elements  with width equal to the width of the input. Each processing element passes state with its neighbor, and the circuit processes an entire row of input at a time. This makes the whole circuit both quite elegant to write in Hardcaml and very low latency -- achieving a one cycle per row of input. This may beat day 3 for my favorite solution so far!

![System diagram](images/day7.png)

Each processing element represents a column, and stores the number of paths reaching the column and the number of splits which occurred at the column The update logic is pretty simple:
- Add input from neighbors to number of paths
- If the input is a `^`:
    - Output number of paths to neighbors
    - If number of paths is > 0, increment number of splits
- If the input is an `S`, increment number of paths

Then the part 1 output is the sum of splits across each processing element, and the part 2 output is the sum of path counts

**Performance**: Executes in 142 cycles (1 per line)

## Usage

### Setup
Assuming you already have OCaml installed, create a switch with `opam switch create hardcaml 5.2.1` then run `opam install hardcaml ppx_hardcaml hardcaml_waveterm`

### Running Tests

You can run the tests for a specific day using `dune test`. For example, to test Day 1:

```bash
dune test src/Day1
```

### Running the solution
To run the solution against the input file, use `dune exec`. For example, for Day 1:
```bash
dune build
dune exec day1
```