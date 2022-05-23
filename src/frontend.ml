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

  let rec launch () =
    match ArgParser.parse () with
    | Error (`UnknownSubCmd _ as err) -> Handler.handle_err @@ `CommonErr err
    | Error (`Usage msg) -> Handler.handle_text msg
    | Ok (History { mode }) -> (
        match mode with
        | `Del re -> Lwt.return @@ Hist.del_from_regex re
        | `Display -> hist_display ()
        | `Search re -> hist_search re)
    | Ok (Search { address; raw; certificate }) ->
        search ~address ~raw ~certificate

  and hist_display () = LTerm.printlf "%s%!" @@ Hist.(show @@ get_entries ())

  and hist_search re =
    LTerm.printlf "%s%!" @@ Hist.(show @@ search_from_regex re)

  and search ~address ~raw ~certificate =
    match address with
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
