module type S = sig
  val handle : string -> mime:string -> unit Lwt.t
end

open Lwt.Syntax

let download prompt body mime =
  let* path = prompt "Download under: " in
  Lwt_io.with_file ~mode:Output
    (path ^ "." ^ mime)
    (fun outc -> Lwt_io.write outc body)

module Make (Prompt : Prompt.S) : S = struct
  let open_default body mime =
    let* fname, outc =
      Lwt_io.open_temp_file ~prefix:"indigobi_" ~suffix:("." ^ mime) ()
    in
    let* () = Lwt_io.write outc body in
    let launch_app =
      [|
        [%system { darwin = "open"; default = "xdg-open"; win32 = "start" }];
        fname;
      |]
    in
    let* _ = Lwt_process.exec ~stderr:`Close ("", launch_app) in
    Lwt_io.close outc

  let handle body ~mime =
    if%lwt Prompt.prompt_bool "Open file in its default app?" then
      open_default body mime
    else download Prompt.prompt body mime
end

module MakeMirage (Prompt : Prompt.S) = struct
  let handle body ~mime = download Prompt.prompt body mime
end
