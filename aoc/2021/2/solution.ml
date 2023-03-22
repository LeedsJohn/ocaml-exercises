open Base

type command = {direction: string; distance: int}

let commands = 
    let file = Stdio.In_channel.create "input.txt" in
    let get_command line = 
        let line = String.split line ~on:' ' in
        let dir = List.hd_exn line in
        let dist = List.tl_exn line |> List.hd_exn |> Int.of_string in
        {direction = dir; distance = dist}
    in
    let res = List.map ~f:get_command (Stdio.In_channel.input_lines file) in
    In_channel.close file;
    res

let part1 commands = 
    let rec aux x y = function
        | [] -> x * y
        | (h :: t) -> match h.direction with
            | "down" -> aux x (y + h.distance) t
            | "up" -> aux x (y - h.distance) t
            | _ -> aux (x + h.distance) y t (* forward *)
    in
    aux 0 0 commands

let part2 commands = 
    let rec aux x y aim = function
        | [] -> x * y
        | (h :: t) -> match h.direction with
            | "down" -> aux x y (aim + h.distance) t
            | "up" -> aux x y (aim - h.distance) t
            | _ -> aux (x + h.distance) (y + h.distance * aim) aim t (* forward *)
    in
    aux 0 0 0 commands

let () = Stdio.print_endline ("Part 1: " ^ (Int.to_string (part1 commands)))
let () = Stdio.print_endline ("Part 2: " ^ (Int.to_string (part2 commands)))
