open Hardware_day5.Hardware

let () = Stdio.printf "part1_ideal=635, part2_ideal=369761800782619\n"

let file = "inputs/day5.txt"
let content = In_channel.with_open_text file In_channel.input_lines

let (ranges, ids) = List.fold_left (fun (ranges, ids) line ->
    match String.split_on_char '-' line with
      | [l ; r] -> ((int_of_string l, int_of_string r) :: ranges, ids)
      | [id] when id <> "" -> (ranges, (int_of_string id) :: ids)
      | _ -> (ranges, ids)
) ([], []) content

let () = testbench ranges ids false