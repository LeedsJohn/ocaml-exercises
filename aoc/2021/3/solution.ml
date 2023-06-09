open Base

let fname = "input.txt"
;;

let numbers = 
    let file = Stdio.In_channel.create fname in
    let get_bits line = Array.map ~f:(fun c -> String.of_char c |> Int.of_string) (String.to_array line)
    in
    let res = List.map ~f:get_bits (Stdio.In_channel.input_lines file) in
    In_channel.close file;
    res
;;

let m = Array.length (List.hd_exn numbers)
;;

let most_common_bit nums i  =
    let sum = List.fold
        (List.map nums ~f:(fun ar -> ar.(i)))
        ~f:(+) ~init:0
    in
    if sum * 2 >= (List.length nums) then 1 else 0
;;

let least_common_bit nums i = if (most_common_bit nums i) = 1 then 0 else 1
;;

let bin_to_dec bin =
    List.fold
        (List.mapi bin ~f:(fun i n -> n * (Int.pow 2 (m - 1 - i))))
        ~f:(+) ~init:0
;;

let gamma = List.map (List.range ~stride:1 0 m) ~f:(most_common_bit numbers)
;;

let epsilon = List.map gamma ~f:(fun n -> if n = 0 then 1 else 0)
;;

let part1 = (bin_to_dec gamma) * (bin_to_dec epsilon)

let eliminate get_goal_bit i nums =
    let goal_bit = get_goal_bit nums i
    in
    List.filter nums ~f:(fun ar -> ar.(i) = goal_bit)
;;

let rating eliminate_func =
    let rec aux i nums =
        let filtered = eliminate_func i nums in
        match filtered with
        | [h] -> Array.to_list h
        | _   -> aux (i + 1) filtered
    in
    aux 0 numbers
;;

let oxygen = rating (eliminate most_common_bit)
;;

let carbon_dioxide = rating (eliminate least_common_bit)
;;

let part2 = (bin_to_dec oxygen) * (bin_to_dec carbon_dioxide)

let () = Stdio.printf "\nPart 1: %d\n" part1

let () = Stdio.printf "\nPart 2: %d\n" part2


(* open Base

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


let () = Stdio.print_endline ("Part 1: " ^ (Int.to_string (part1 (numbers "input.txt"))))
let () = Stdio.print_endline ("Part 2: " ^ (Int.to_string (part2 commands))) *)