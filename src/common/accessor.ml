module type S = sig
  val read : unit -> string option Lwt.t
  val write : string -> unit Lwt.t
end

module Location = struct
  type t = Cache | Config

  let of_string t =
    Filename.concat (Sys.getenv "HOME")
    @@
    match t with
    | Cache ->
        Filename.concat
          [%system { darwin = "Library/Caches"; unix = ".cache" }]
          [%system { darwin = "Indigobi"; unix = "indigobi" }]
    | Config ->
        [%system
          {
            darwin = "Library/Preferences/indigobi.plist";
            unix = ".config/indigobi";
          }]
end

let make loc fname =
  let loc = Location.of_string loc in
  let path = Filename.concat loc fname in
  (module struct
    open Lwt.Syntax

    let read () =
      Lwt.catch
        (fun () ->
          Lwt_io.with_file path ~mode:Input (fun inc ->
              let* content = Lwt_io.read inc in
              Lwt.return_some content))
        (function
          | Unix.Unix_error (ENOENT, _, _) ->
              let* test = Lwt_unix.file_exists loc in
              let* () =
                if test then Lwt.return_unit else Lwt_unix.mkdir loc 0o751
              in
              let* () =
                Lwt_io.with_file path ~mode:Output (fun _ ->
                    Log.infof "Create %s" fname)
              in
              Lwt.return_none
          | exn -> Lwt.fail exn)

    let write str =
      Lwt_io.with_file path ~mode:Output (fun outc -> Lwt_io.write outc str)
  end : S)
