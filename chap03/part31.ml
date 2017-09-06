open Core

let is_ocaml_source s =
  match String.rsplit2 s ~on:'.' with
  | Some (_,("ml"|"mli")) -> true
  | _ -> false

let (ml_files,other_files) =
  List.partition_tf (Sys.ls_dir ".") ~f:is_ocaml_source