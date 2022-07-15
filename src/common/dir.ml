module Dirs (App : sig
  val app_name : string
end) =
struct
  open Lwt.Syntax

  type os_type = Darwin | OtherUnix

  let os_type =
    Lwt_main.run
      (let* system = Lwt_process.pread ("", [| "uname" |]) in
       Lwt.return @@ match system with "Darwin" -> Darwin | _ -> OtherUnix)

  let in_home fmt args =
    Filename.concat (Sys.getenv "HOME") (Printf.sprintf fmt args)

  let cache_dir =
    match os_type with
    | Darwin ->
        in_home "Library/Caches/%s" (String.capitalize_ascii App.app_name)
    | OtherUnix -> in_home ".cache/%s" App.app_name

  let config_dir =
    match os_type with
    | Darwin -> in_home "Library/Preferences/%s.plist" App.app_name
    | OtherUnix -> in_home ".config/%s" App.app_name
end

include Dirs (struct
  let app_name = "indigobi"
end)
