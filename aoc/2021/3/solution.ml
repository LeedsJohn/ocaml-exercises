open Base

let numbers fname = 
    let file = Stdio.In_channel.create fname in
    let get_bits line = Array.map ~f:(fun c -> String.of_char c |> Int.of_string) (String.to_array line)
    in
    let res = List.map ~f:get_bits (Stdio.In_channel.input_lines file) in
    In_channel.close file;
    res

let bin_to_dec bin =
    let rec aux power i = if i = -1 then 0 else bin.(i) * power + aux (power * 2) (i - 1)
    in
    aux 1 ((Array.length bin) - 1)

let invert bin = Array.map bin ~f:(fun x -> abs (x - 1))

let part1 nums =
    let length = Array.length (List.hd_exn nums) in
    let goal = (List.length nums) / 2 in
    let res = Array.create ~len:length 0 in (* number of 1 bits in each position *)
    let get_num_ones ar = for i = 0 to (length - 1) do res.(i) <- res.(i) + ar.(i) done in 
    List.iter nums ~f:get_num_ones;
    let get_bit n = if n > goal then 1 else 0 in
    let res = Array.map res ~f:get_bit in
    (bin_to_dec res) * (bin_to_dec (invert res))

let part2 nums =
    let most_common ar =


let () = Stdio.print_endline ("Part 1: " ^ (Int.to_string (part1 (numbers "input.txt"))))
(* let () = Stdio.print_endline ("Part 2: " ^ (Int.to_string (part2 commands))) *)
