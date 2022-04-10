open Import

module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make (Backend : Backend.S) (Handler : Handler.S) (ArgParser : Cli.S) :
  S = struct
  open Lwt.Syntax

  let make_history () : (module History.S) =
    match Dir.cache_dir with
    | None -> failwith "No history path"
    | Some path ->
        (module History.Make (struct
          let path = path
        end))

  module Hist = (val make_history ())

  let rec launch () =
    match ArgParser.parse () with
    | Error (`UnknownSubCmd _ as err) -> Handler.handle_err @@ `CommonErr err
    | Error (`Usage msg) -> Handler.handle_text msg
    | Ok (History { regexp }) -> history_search regexp
    | Ok (Search { adresss; raw; certificate }) ->
        search ~adresss ~raw ~certificate

  and history_search = function
    | None ->
        let* () = Hist.iter_s (fun e -> Lwt_io.printf "%s\n" e) in
        Lwt_io.(flush stdout)
    | Some re ->
        let* entries = Hist.search_from_regex re in
        let* () = Lwt_list.iter_s (fun e -> Lwt_io.printf "%s\n" e) entries in
        Lwt_io.(flush stdout)

  and search ~adresss ~raw ~certificate =
    match adresss with
    | None -> Handler.handle_err @@ `CommonErr `NoUrlProvided
    | Some adrr -> (
        Hist.add_entry adrr;
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
        | Error (#G.Status.err as e) -> Handler.handle_err @@ `GeminiErr e
        | Error (#Err.t as e) -> Handler.handle_err @@ `CommonErr e)
end
