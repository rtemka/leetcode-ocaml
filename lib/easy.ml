(* PROBLEM *)
(* https://leetcode.com/problems/running-sum-of-1d-array *)
let rec running_sum_tr list ~acc_list ~sum =
  match list with
  | [] -> acc_list
  | hd :: tl ->
      let sum_so_far = hd + sum in
      running_sum_tr tl ~acc_list:(sum_so_far :: acc_list) ~sum:sum_so_far

(* running_sum runs helper funciton running_sum_tr and reverses result *)
let running_sum_of_1d_array list =
  List.rev (running_sum_tr list ~acc_list:[] ~sum:0)

(* test that expects bool output, kinda of silent assert *)
let%test "running_sum_of_1d_array" =
  let got = running_sum_of_1d_array [ 1; 2; 3 ] in
  let want = [ 1; 3; 6 ] in
  List.equal Int.equal got want

(* PROBLEM richest customer wealth *)
(* https://leetcode.com/problems/richest-customer-wealth/description/ *)
let richest_customer_wealth list =
  list |> List.map (List.fold_left ( + ) 0) |> List.fold_left Int.max 0

let%test "richest_customer_wealth" =
  let got =
    [
      richest_customer_wealth [ [ 1; 2; 3 ]; [ 3; 2; 1 ] ];
      richest_customer_wealth [ [ 1; 5 ]; [ 7; 3 ]; [ 3; 5 ] ];
    ]
  in
  let want = [ 6; 10 ] in
  List.equal Int.equal got want

(* PROBLEM fizz_buzz *)
(* https://leetcode.com/problems/fizz-buzz/description/ *)
let fizz_buzz n =
  if n mod 15 = 0 then "FizzBuzz"
  else if n mod 3 = 0 then "Fizz"
  else if n mod 5 = 0 then "Buzz"
  else Int.to_string n

let fizz_buzz_list n =
  let (_ :: tl) = List.init (n + 1) (fun n -> fizz_buzz n) in
  tl

let%test "fizz_buzz_list" =
  let got = [ fizz_buzz_list 3; fizz_buzz_list 5; fizz_buzz_list 15 ] in
  let want =
    [
      [ "1"; "2"; "Fizz" ];
      [ "1"; "2"; "Fizz"; "4"; "Buzz" ];
      [
        "1";
        "2";
        "Fizz";
        "4";
        "Buzz";
        "Fizz";
        "7";
        "8";
        "Fizz";
        "Buzz";
        "11";
        "Fizz";
        "13";
        "14";
        "FizzBuzz";
      ];
    ]
  in
  List.equal (List.equal String.equal) got want
