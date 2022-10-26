module type S = sig
  val read : unit -> Sexplib.Sexp.t option Lwt.t
  val write : string -> unit Lwt.t
end

module Location = struct
  type t = Cache | Config

  let home_dir =
    Sys.getenv [%system { default = "HOME"; win32 = "USERPROFILE" }]

  let cache_dir =
    Filename.concat
      [%system
        {
          darwin = "Library/Caches";
          default = ".cache";
          win32 = "AppData\\Local\\Microsoft\\Windows";
        }]
      [%system { darwin = "Indigobi"; unix = "indigobi"; win32 = "INetCache" }]

  let config_dir =
    [%system
      {
        darwin = "Library/Preferences/indigobi.plist";
        unix = ".config/indigobi";
        win32 = "AppData\\Local";
      }]

  let of_string t =
    Filename.concat home_dir
    @@ match t with Cache -> cache_dir | Config -> config_dir
end

let make loc fname =
  let loc = Location.of_string loc in
  let path = Filename.concat loc fname in
  (module struct
    open Lwt.Syntax

    let touch_dir_if_non_existant () =
      if%lwt Lwt_unix.file_exists loc then Lwt.return_unit
      else
        let* () = Lwt_unix.mkdir loc 0o751 in
        Log.infof "Create %S" loc

    let read () =
      try%lwt
        Lwt_io.with_file path ~mode:Input (fun inc ->
            let* content = Lwt_io.read inc in
            Lwt.return_some @@ Sexplib.Sexp.of_string content)
      with
      | Unix.Unix_error (ENOENT, _, _) ->
          let* () = touch_dir_if_non_existant () in
          let* () =
            Lwt_io.with_file path ~mode:Output (fun _ ->
                Log.infof "Create %S" path)
          in
          Lwt.return_none
      | Failure _ | Sexplib.Sexp.Parse_error _ ->
          let* () = Log.errf "%S is corrupted" fname in
          Lwt.return_none

    let write str =
      let* () = touch_dir_if_non_existant () in
      Lwt_io.with_file path ~mode:Output (fun outc -> Lwt_io.write outc str)
  end : S)