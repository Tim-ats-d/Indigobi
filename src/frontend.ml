open Import

module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make (Backend : Backend.S) (Handler : Handler.S) (ArgParser : Cli.S) :
  S = struct
  module Dir = Directories.Project_dirs (struct
    let qualifier = "com"
    and organization = "indigobi"
    and application = "indigobi"
  end)

  open Lwt.Syntax

  let launch () =
    match Dir.cache_dir with
    | None -> Handler.handle_warning `NoHistoryPath
    | Some path -> (
        let module Hist =
          History.Make
            (struct
              let path = path
            end)
            (History.Default.Entry)
        in
        match ArgParser.parse () with
        | Error (`UnknownSubCmd _ as err) ->
            Handler.handle_err @@ `CommonErr err
        | Error (`Usage msg) -> Handler.handle_text msg
        | Ok (Search { adresss; raw; certificate }) -> (
            match adresss with
            | None ->
                let* () = Handler.handle_err @@ `CommonErr `NoUrlProvided in
                Lwt.return ()
            | Some adrr -> (
                Hist.add_entry @@ History.Default.Entry.from_string adrr;
                let url = Urllib.parse adrr "" in
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
                | Error (#G.Status.err as e) ->
                    Handler.handle_err @@ `GeminiErr e
                | Error (#Err.t as e) -> Handler.handle_err @@ `CommonErr e)))
end
