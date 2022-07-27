open Import

module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make (ArgParser : Cli.S) : S = struct
  module HistEntry = struct
    open Sexplib.Conv

    type t = Lib.Url.t = {
      scheme : string;
      domain : string;
      port : int;
      path : string;
      query : string;
    }
    [@@deriving eq, sexp]

    let from_string = Fun.flip Lib.Url.parse ""
    let to_string = Lib.Url.to_string
    let show = to_string
  end

  let hist = History.create ~fname:"history" (module HistEntry)

  open Lwt.Syntax

  let search (module Back : Backend.S) (module Handler : Handler.S) addr ~raw
      ~certificate =
    let url = Lib.Url.parse addr "" in
    let* result =
      Back.get_page ~url:(Lib.Url.to_string url) ~host:url.domain ~port:url.port
        ~cert:certificate
    in
    match result with
    | Ok ({ Mime.media_type = Gemini; _ }, body) ->
        if raw then Handler.handle_text body
        else
          Handler.handle_gemini
            Front.Context.{ current_url = url; history = hist }
          @@ Gemini.Gemtext.parse body
    | Ok ({ Mime.media_type = Text txt; _ }, body) ->
        Handler.handle_text body ~typ:txt
    | Ok ({ Mime.media_type = Other mime; _ }, body) ->
        Handler.handle_other body ~mime
    | Error (#Gemini.Status.err as e) -> Handler.handle_err @@ `GeminiErr e
    | Error (#Err.t as e) -> Handler.handle_err @@ `CommonErr e

  let launch () =
    let module Printer = Printer.Make ((val Config.ThemeManager.get ())) in
    let module Handler = Handler.Make (Printer) in
    let module Back = Backend.Make (Prompt.Make (Printer)) (Requester.Default)
    in
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
            Lwt.finalize
              (fun () ->
                let timeout = Lwt_unix.sleep timeout in
                Lwt.pick
                  [
                    timeout;
                    search (module Back) (module Handler) addr ~raw ~certificate;
                  ])
              (fun () -> History.push hist @@ HistEntry.from_string addr))
end
