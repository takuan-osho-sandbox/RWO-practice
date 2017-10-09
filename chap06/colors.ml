open Core_kernel;;

type basic_color =
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White ;;

let basic_color_to_int = function
  | Black -> 0 | Red     -> 1 | Green -> 2 | Yellow -> 3
  | Blue  -> 4 | Magenta -> 5 | Cyan  -> 6 | White  -> 7 ;;

let color_by_number number text =
  sprintf "\027[38;5;%dm%s\027[0m" number text

type weight = Regular | Bold

type color =
  | Basic of basic_color           (* basic colors *)
  | Bold  of basic_color           (* bold basic colors *)
  | RGB   of int * int * int       (* 6x6x6 color cube *)
  | Gray  of int                   (* 24 grayscale levels *)

let color_to_int = function
  | Basic basic_color -> basic_color_to_int basic_color
  | Bold basic_color -> 8 + basic_color_to_int basic_color
  | RGB (r, g, b) -> 16 + b + g * 6 + r * 36
  | Gray i -> 232 + i

let color_print color s =
  printf "%s\n" (color_by_number (color_to_int color) s)

let () = color_print (Basic Red) "A bold red!"
let () = color_print (Gray 4) "A muted gray..."