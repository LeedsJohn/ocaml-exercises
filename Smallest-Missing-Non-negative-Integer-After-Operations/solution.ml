open Base

let modulo x y = if x = 0 then 0 else if x > 0 then x % y else (x * -1) % y

let findSmallestInteger nums value = 
    let times_seen = Array.create ~len:(value + 1) 0 in
    let numbers = Array.create ~len:((Array.length nums) + 1) false in
    let set_val n =
        let n = modulo n value
        in 
        let res = n + (value * times_seen.(n)) in
        times_seen.(n) <- times_seen.(n) + 1;
        if res < (Array.length numbers) then
            numbers.(res) <- true else ()
    in
    let () = Array.iter nums ~f:set_val in
    let rec find_answer i =
        match numbers.(i) with
        | false -> i
        | true -> find_answer (i + 1)
    in
    find_answer 0
(* -------------------------------------------------------------------------- *)
let () = Stdio.print_endline "Testing:"

let tests = [|
    (1, [|1; -10; 7; 13; 6; 8|], 5, 4);
    (2, [|1; -10; 7; 13; 6; 8|], 7, 2);
    (3, [|0; 0; 0;|], 0, 1)
|]

let test case =
    let case_num, nums, value, expected = case in
    if findSmallestInteger nums value <> expected then 
        "Failed test " ^ Int.to_string case_num |> Stdio.print_endline else ()

let () = Array.iter tests ~f:test
