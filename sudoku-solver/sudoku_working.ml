open Base

let () = Stdio.print_endline ""

let is_full board =
    let rec aux x y =
        if y = 9 then true else
        if x = 9 then aux 0 (y + 1) else
        if board.(y).(x) = 0 then false else aux (x + 1) y
    in
    aux 0 0

let get_topleft_corner x y =
    let get_pos n = (n / 3) * 3
    in
    (get_pos x, get_pos y)

let valid_nums board x y =
    let nums = Array.create ~len:10 true in
    for nx = 0 to 8 do nums.(board.(y).(nx)) <- false done;
    for ny = 0 to 8 do nums.(board.(ny).(x)) <- false done;
    let (x1, y1) = get_topleft_corner x y in
    for nx = x1 to x1 + 2 do
        for ny = y1 to y1 + 2 do
            nums.(board.(ny).(nx)) <- false
        done;
    done;
    let rec aux acc i =
        if i = 10 then acc else
        if nums.(i) then aux (i :: acc) (i + 1) else aux acc (i + 1)
    in
    aux [] 1

type move_info = {options: int; x: int; y: int}

let get_move board =
    let rec aux res x y = (* this can't be a good way to iterate through a 2d matrix *)
        if y = 9 then res else
        if x = 9 then aux res 0 (y + 1) else
        if board.(y).(x) <> 0 then aux res (x + 1) y else
        let num_moves = valid_nums board x y |> List.length in
        if num_moves < res.options then aux {options = num_moves; x = x; y = y} (x + 1) y else
        aux res (x + 1) y
    in
    aux {options = 10; x = -1; y = -1} 0 0

let show_board board =
    for y = 0 to 8 do
        for x = 0 to 8 do
            Stdio.print_string ((Int.to_string board.(y).(x)) ^ " ")
        done;
        Stdio.print_endline ""
    done

(* returns if the board is solved *)
let solve board =
    let rec aux () = (* is this a bandaid solution for having 0 arguments? *)
        if is_full board then true else 
        let move = get_move board in
        let choices = valid_nums board move.x move.y in
        let rec try_moves = function
            | [] -> false
            | (h :: t) ->
                board.(move.y).(move.x) <- h;
                if aux () then true else
                    let () = board.(move.y).(move.x) <- 0 in try_moves t
        in
        try_moves choices
    in
    aux ()


let puzzle = [|
    [|0; 0; 7; 5; 2; 8; 0; 6; 9|];
    [|9; 4; 0; 0; 0; 7; 0; 0; 2|];
    [|6; 0; 0; 0; 0; 4; 7; 0; 1|];
    [|3; 0; 1; 0; 5; 0; 0; 7; 0|];
    [|0; 0; 0; 7; 8; 6; 1; 9; 0|];
    [|7; 8; 0; 3; 0; 0; 0; 2; 0|];
    [|0; 1; 9; 0; 0; 0; 2; 0; 5|];
    [|0; 7; 3; 0; 9; 0; 6; 0; 0|];
    [|0; 0; 5; 8; 4; 0; 0; 0; 0|];
    |]

let _ = solve puzzle

let () = show_board puzzle
