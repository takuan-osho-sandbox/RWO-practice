open Core

let rec length_plus_n l n =
  match l with
  | [] -> n
  | _ :: tl -> length_plus_n tl (n + 1)

let length l = length_plus_n l 0

let make_list n = List.init n ~f:(fun x -> x)