open Core;;

List.filter_map (Sys.ls_dir ".") ~f:(fun fname ->
  match String.rsplit2 ~on:'.' fname with
    | None | Some ("",_) -> None
    | Some (_,ext) -> Some ext)
  |> List.dedup