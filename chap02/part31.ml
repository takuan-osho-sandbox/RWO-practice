open Core

let (|>) x f = f x

let path = "/usr/bin:/usr/local/bin:/bin:/sbin";;

String.split ~on:':' path
|> List.dedup ~compare:String.compare
|> List.iter ~f:print_endline