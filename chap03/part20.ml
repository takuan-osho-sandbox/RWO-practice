open Core

let max_widths header rows =
  let lengths l = List.map ~f:String.length l in
  List.fold rows ~init:(lengths header)
                 ~f:(fun acc row -> List.map2_exn ~f:Int.max acc (lengths row))

let render_separator widths =
  let pieces = List.map widths
    ~f:(fun w -> String.make (w + 2) '-') in
    "|" ^ String.concat ~sep:"+" pieces ^ "|"

let s = render_separator [3;6;2]

let pad s length =
  " " ^ s ^ String.make (length - String.length s + 1) ' '

let p = pad "hello" 10

let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "|" ^ String.concat ~sep:"|" padded ^ "|"

let rr = render_row ["Hello";"World"] [10;15];;