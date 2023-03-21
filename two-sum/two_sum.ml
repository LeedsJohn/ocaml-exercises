open Base

let two_sum nums goal =
    let rec aux appeared = function
        | [] -> false
        | (h :: t) -> if Set.mem appeared (goal - h) then true else
            aux (Set.add appeared h) t
    in
    aux (Set.empty (module Int)) nums
(* -------------------------------------------------------------------------- *)
let () = Stdio.print_endline "Testing:"

let tests = [
    ([1; 2; 3; 4], 6, true);
    ([1; 2; 3; 4], 17, false);
    ([], 0, false);
    ([1; 1; 1; 1], 2, true);
    ([0; 5; 19; 3490; 34; 4328; 4932; 17], 17, true)
] 

let _ =
    let rec aux i = function
        | [] -> ()
        | ((nums, goal, ans) :: t) -> if (Bool.to_int (two_sum nums goal)) = (Bool.to_int ans) then aux (i + 1) t else
            Stdio.print_endline ("Failed test " ^ (Int.to_string i));
            aux (i + 1) t
    in
    aux 1 tests
