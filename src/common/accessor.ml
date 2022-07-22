module type S = sig
  val read : unit -> string option Lwt.t
  val write : string -> unit Lwt.t
end

open Lwt.Syntax

let make path =
  (module struct
    let read () =
      Lwt.catch
        (fun () ->
          Lwt_io.with_file path ~mode:Input (fun inc ->
              let* content = Lwt_io.read inc in
              Lwt.return_some content))
        (function
          | Unix.Unix_error (ENOENT, _, _) ->
              let* () =
                Lwt_io.with_file path ~mode:Output (fun _ ->
                    Log.infof "Create %s" path)
              in
              Lwt.return_none
          | exn -> Lwt.fail exn)

    let write str =
      Lwt_io.with_file path ~mode:Output (fun outc -> Lwt_io.write outc str)
  end : S)
