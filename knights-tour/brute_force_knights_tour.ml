module Knight = struct
    type knight = 
        | Pos of int * int;;

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
    (* Future: make the board nxn instead of 6x6 *)
    (* Represents which spaces have been visited *)
    let board = Array.make_matrix 6 6 false 

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
        if length = 36 then Some path else
        let rec next_moves = function
            | [] -> None
            | ((nx, ny) :: t) ->
                Board.set nx ny true;
                match aux (Knight.Pos (nx, ny)) ((nx, ny) :: path) (length + 1) with
                | None -> 
                    Board.set nx ny false;
                    next_moves t
                | Some p -> Some p
        in 
        next_moves (Board.valid_moves kn)
    in 
    let res = aux (Knight.Pos (x, y)) [(x, y)] 1 in
    match res with
    | None -> None
    | Some res -> Some (List.rev res)

let path = knights_tour 0 0
let () = show_path path
