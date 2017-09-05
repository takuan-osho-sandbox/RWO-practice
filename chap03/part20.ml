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