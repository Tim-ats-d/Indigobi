open Import

module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make (Backend : Backend.S) (Printer : Printer.S) (ArgParser : Cli.S) :
  S = struct
  let launch () =
    match ArgParser.parse () with
    | Error (`UnknownSubCmd _ as err) -> Printer.handle_err @@ `CommonErr err
    | Error (`Usage msg) -> Printer.handle_text msg
    | Ok (Search { adresss; raw; certificate }) -> (
        let url =
          match adresss with
          | None ->
              Lwt_main.run @@ Printer.handle_err @@ `CommonErr `NoUrlProvided;
              exit 1
          | Some adrr -> Common.Urllib.parse adrr ""
        in
        match
          Backend.get
            ~url:(Common.Urllib.to_string url)
            ~host:url.domain ~port:url.port ~cert:certificate
        with
        | Ok ({ Mime.media_type = `Gemini; _ }, body) ->
            if raw then Printer.handle_text body
            else Printer.handle_gemini @@ Gemini.Text.parse body
        | Ok ({ Mime.media_type = `Text txt; _ }, body) ->
            Printer.handle_text body ~typ:txt
        | Ok ({ Mime.media_type = `Other o; _ }, body) ->
            Printer.handle_other o body
        | Error (#G.Status.err as e) -> Printer.handle_err @@ `GeminiErr e
        | Error (#Common.Err.t as e) -> Printer.handle_err @@ `CommonErr e)
end
