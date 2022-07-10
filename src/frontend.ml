open Import

module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make (Backend : Backend.S) (Handler : Handler.S) (ArgParser : Cli.S) :
  S = struct
  let make_history () : (module History.S) =
    match Dir.cache_dir with
    | None -> failwith "No history path"
    | Some path ->
        (module History.Make (struct
          let path = path
        end))

  module Hist = (val make_history ())
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
        else Handler.handle_gemini @@ Gemini.Text.parse body
    | Ok ({ Mime.media_type = Text txt; _ }, body) ->
        Handler.handle_text body ~typ:txt
    | Ok ({ Mime.media_type = Other _; _ }, body) -> Handler.handle_other body
    | Error (#Gemini.Status.err as e) -> Handler.handle_err @@ `GeminiErr e
    | Error (#Err.t as e) -> Handler.handle_err @@ `CommonErr e

  let launch () =
    let* _ = Handler.handle_other "i herd u liek mudkipz" in

    (* todo: remove this *)
    match ArgParser.parse () with
    | Error ((`CliErrUnknownSubCmd _ | `CliErrBadTimeoutFormat) as err) ->
        Handler.handle_err @@ `CommonErr err
    | Error (`CliErrUsageMsg msg) -> Handler.handle_text msg
    | Ok (History { mode = `Del re }) -> Lwt.return @@ Hist.del_from_regex re
    | Ok (History { mode = `Display }) ->
        Hist.get_all () |> Hist.show |> LTerm.printlf "%s%!"
    | Ok (History { mode = `Search re }) ->
        Hist.search_from_regex re |> Hist.show |> LTerm.printlf "%s%!"
    | Ok (Search { address; raw; certificate; timeout }) -> (
        match address with
        | None -> Handler.handle_err @@ `CommonErr `NoUrlProvided
        | Some addr ->
            Hist.push addr;
            let timeout = Lwt_unix.sleep timeout in
            Lwt.pick [ timeout; search addr ~raw ~certificate ])
end
