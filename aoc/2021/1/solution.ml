open Base

let numbers = 
    let file = Stdio.In_channel.create "input.txt" in
    let res = List.map ~f:Int.of_string (Stdio.In_channel.input_lines file) in
    In_channel.close file;
    res

let extract_int o =
    match o with
    | None -> 0
    | Some n -> n

let extract_list o =
    match o with
    | None -> []
    | Some l -> l

let part1 nums =
    let rec aux prev = function
        | [] -> 0
        | (h :: t) -> let greater = if h > prev then 1 else 0 in greater + aux h t
    in
    aux (extract_int (List.hd nums)) (extract_list (List.tl nums))

let part2 nums = 
    let numbers = Array.of_list nums in
    let rec aux acc i =
        if i + 2 = Array.length numbers then acc else aux ((numbers.(i) + numbers.(i + 1) + numbers.(i + 2)) :: acc) (i + 1)
    in
    let windows = List.rev (aux [] 0) in
    part1 windows

let () = "Part 1: " ^ (Int.to_string (part1 numbers)) |> Stdio.print_endline

let () = "Part 2: " ^ (Int.to_string (part2 numbers)) |> Stdio.print_endline

