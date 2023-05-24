open Base

let fname = "input2.txt"
;;


let numbers = 
  let file = Stdio.In_channel.create fname in
  let lines = Stdio.In_channel.input_lines file in
  let nums = String.split (List.hd_exn lines) ~on:','
  |> List.map ~f:(Int.of_string) in
  let rec get_board lines board = 
    match lines with
    | []        -> board
    | ("" :: _) -> board
    | (h :: t)  -> Array.to_

;;



let () = List.iter numbers ~f:(fun n -> Stdio.printf "%d\n" n)