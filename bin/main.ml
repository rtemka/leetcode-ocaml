open Printf

(* main *)
let () = List.iter (printf "%d\n") (Easy.running_sum_of_1d_array [ 1; 3; 6 ])
