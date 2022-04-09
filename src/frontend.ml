open Import

module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make (Backend : Backend.S) (Handler : Handler.S) (ArgParser : Cli.S) :
  S = struct
  let launch () =
    match ArgParser.parse () with
    | Error (`UnknownSubCmd _ as err) -> Handler.handle_err @@ `CommonErr err
    | Error (`Usage msg) -> Handler.handle_text msg
    | Ok (Search { adresss; raw; certificate }) -> (
        let url =
          match adresss with
          | None ->
              Lwt_main.run @@ Handler.handle_err @@ `CommonErr `NoUrlProvided;
              exit 1
          | Some adrr -> Urllib.parse adrr ""
        in
        match
          Backend.get ~url:(Urllib.to_string url) ~host:url.domain
            ~port:url.port ~cert:certificate
        with
        | Ok ({ Mime.media_type = `Gemini; _ }, body) ->
            if raw then Handler.handle_text body
            else Handler.handle_gemini @@ Gemini.Text.parse body
        | Ok ({ Mime.media_type = `Text txt; _ }, body) ->
            Handler.handle_text body ~typ:txt
        | Ok ({ Mime.media_type = `Other o; _ }, body) ->
            Handler.handle_other o body
        | Error (#G.Status.err as e) -> Handler.handle_err @@ `GeminiErr e
        | Error (#Err.t as e) -> Handler.handle_err @@ `CommonErr e)
end
