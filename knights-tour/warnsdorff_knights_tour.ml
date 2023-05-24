module Knight = struct
    type knight = 
        | Pos of int * int

    let get_coords = function
        | Pos (x, y) -> (x, y)

    (* all possible moves for the knight regardless of if they're legal *)
    let get_moves kn = 
        let offsets = [(2, 1); (2, -1); (-2, 1); (-2, -1); (1, 2); (-1, 2); (1, -2); (-1, -2)]
        in
        let (x, y) = get_coords kn
        in
        let rec aux acc = function
            | [] -> acc
            | (nx, ny) :: t -> aux ((x + nx, y + ny) :: acc) t
        in
        aux [] offsets
end

module Board = struct
    (* Future: make the board nxn instead of 100x100 *)
    (* Represents which spaces have been visited *)
    let board = Array.make_matrix 100 100 false 

    let get x y = board.(x).(y)

    let set x y value = board.(x).(y) <- value

    (* Filters out of bound moves *)
    (* probably could use List.filter or something? *)
    let valid_moves kn =
        let moves = Knight.get_moves kn in
        let length = Array.length board in
        let check_pos (x, y) = x >= 0 && x < length && y >= 0 && y < length && not (get x y)
        in
        let rec aux acc = function
            | [] -> acc
            | h :: t -> if check_pos h then aux (h :: acc) t else aux acc t
        in
        aux [] moves
    
    (* warnsdorff implementation picks the next square with the fewest
       possibilities after moving to that square *)
    let pick_move kn = 
        let moves = valid_moves kn in
        let rec aux res = function
            | [] -> snd res
            | ((x, y) :: t) -> let num_moves = List.length (valid_moves (Pos (x, y))) in
                if num_moves < fst res then aux (num_moves, (x, y)) t else aux res t
        in
        aux (10, (-1, -1)) moves
end

let show_path path =
    let rec aux = function
        | [] -> print_endline ""
        | ((x, y) :: t) -> print_string ("(" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ") "); aux t
    in
    match path with
    | None -> ()
    | Some path -> aux path

let knights_tour x y =
    Board.set x y true;
    let rec aux kn path length =
        if length = 10000 then Some path else
            let (nx, ny) = Board.pick_move kn in
            Board.set nx ny true;
            aux (Knight.Pos (nx, ny)) ((nx, ny) :: path) (length + 1)
    in 
    let res = aux (Knight.Pos (x, y)) [(x, y)] 1 in
    match res with
    | None -> None
    | Some res -> Some (List.rev res)

let path = knights_tour 0 0
let () = show_path path
