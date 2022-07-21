open Import

module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make (Backend : Backend.S) (Handler : Handler.S) (ArgParser : Cli.S) :
  S = struct
  open Lwt.Syntax

  let hist =
    History.create ~path:Dir.cache_dir
      (module struct
        include Sexplib.Conv

        type t = string [@@deriving sexp]

        let show t = t
        let to_string t = t
      end)

  let search addr ~raw ~certificate =
    let url = Urllib.parse addr "" in
    let* result =
      Backend.get ~url:(Urllib.to_string url) ~host:url.domain ~port:url.port
        ~cert:certificate
    in
    match result with
    | Ok ({ Mime.media_type = Gemini; _ }, body) ->
        if raw then Handler.handle_text body
        else Handler.handle_gemini ~history:hist @@ Gemini.Text.parse body
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
    | Ok (History { mode = `Del re }) -> History.del_from_regex hist re
    | Ok (History { mode = `Display }) ->
        let* entries = History.get hist in
        LTerm.printlf "%a%!" (History.get_pp_entries hist) entries
    | Ok (History { mode = `Search re }) ->
        let* entries = History.search_from_regex hist re in
        LTerm.printlf "%a%!" (History.get_pp_entries hist) entries
    | Ok (Search { address; raw; certificate; timeout }) -> (
        match address with
        | None -> Handler.handle_err @@ `CommonErr `NoUrlProvided
        | Some addr ->
            let* () = History.push hist addr in
            let timeout = Lwt_unix.sleep timeout in
            Lwt.pick [ timeout; search addr ~raw ~certificate ])
end
