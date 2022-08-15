open Import

module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make (Backend : Backend.S) (Handler : Handler.S) (ArgParser : Cli.S) :
  S = struct
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

  let search addr ~raw_mode ~certificate ~timeout =
    let url = Lib.Url.parse addr "" in
    let* cert = Backend.cert_from_file certificate in
    match cert with
    | Ok c -> (
        let* result =
          Backend.get ~url:(Lib.Url.to_string url) ~host:url.domain
            ~port:url.port ~cert:c ~timeout
        in
        match result with
        | Ok ({ Mime.media_type = Gemini; _ }, body) ->
            if raw_mode then Handler.handle_text body
            else
              Handler.handle_gemini
                Front.Context.{ current_url = url; history = hist }
              @@ Gemini.Gemtext.parse body
        | Ok ({ Mime.media_type = Text txt; _ }, body) ->
            Handler.handle_text body ~typ:txt
        | Ok ({ Mime.media_type = Other mime; _ }, body) ->
            Handler.handle_other body ~mime
        | Error (#Gemini.Status.err as e) -> Handler.handle_err @@ `GeminiErr e
        | Error (#Err.t as e) -> Handler.handle_err @@ `CommonErr e)
    | Error e -> Handler.handle_err @@ `CommonErr e

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
    | Ok (Search { address; raw_mode; certificate; timeout }) -> (
        match address with
        | None -> Handler.handle_err @@ `CommonErr `NoUrlProvided
        | Some addr ->
            Lwt.finalize
              (fun () -> search addr ~raw_mode ~certificate ~timeout)
              (fun () -> History.push hist @@ HistEntry.from_string addr))
end
