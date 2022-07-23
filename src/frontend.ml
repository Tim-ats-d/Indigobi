open Import

module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make (Backend : Backend.S) (Handler : Handler.S) (ArgParser : Cli.S) :
  S = struct
  module Hist = History.Make (struct
    let path = Dir.history_path
  end)

  open Lwt.Syntax

  let search addr ~raw ~certificate =
    let url = Urllib.parse addr "" in
    let* result =
      Backend.get ~url:(Urllib.to_string url) ~host:url.domain ~port:url.port
        ~cert:certificate
    in
    match result with
    | Ok ({ Mime.media_type = Gemini; _ }, body) ->
        if raw then Handler.handle_text body
        else Handler.handle_gemini @@ Gemini.Gemtext.parse body
    | Ok ({ Mime.media_type = Text txt; _ }, body) ->
        Handler.handle_text body ~typ:txt
    | Ok ({ Mime.media_type = Other mime; _ }, body) ->
        Handler.handle_other body ~mime
    | Error (#Gemini.Status.err as e) -> Handler.handle_err @@ `GeminiErr e
    | Error (#Err.t as e) -> Handler.handle_err @@ `CommonErr e

  let launch () =
    match ArgParser.parse () with
    | Error ((`CliErrUnknownSubCmd _ | `CliErrBadTimeoutFormat) as err) ->
        Handler.handle_err @@ `CommonErr err
    | Error (`CliErrUsageMsg msg) -> Handler.handle_text msg
    | Ok (History { mode = `Del re }) -> Hist.del_from_regex re
    | Ok (History { mode = `Display }) ->
        let* hist = Hist.get_all () in
        LTerm.printlf "%a%!" Hist.pp hist
    | Ok (History { mode = `Search re }) ->
        let* hist = Hist.search_from_regex re in
        LTerm.printlf "%a%!" Hist.pp hist
    | Ok (Search { address; raw; certificate; timeout }) -> (
        match address with
        | None -> Handler.handle_err @@ `CommonErr `NoUrlProvided
        | Some addr ->
            let* () = Hist.push addr in
            let timeout = Lwt_unix.sleep timeout in
            Lwt.pick [ timeout; search addr ~raw ~certificate ])
end
