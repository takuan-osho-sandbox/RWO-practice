open Core

let rec ls_rec s =
  if Sys.is_file_exn ~follow_symlinks:true s
    then [s]
    else
      Sys.ls_dir s
        |> List.map ~f:(fun sub -> ls_rec (s ^/ sub))
        |> List.concat