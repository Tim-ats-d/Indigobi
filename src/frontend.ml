open Import

module type S = sig
  val search_and_display : default_url:string -> unit Lwt.t
end

module Make (Cli : Cli.S) (Backend : Backend.S) (Args : Args.S) : S = struct
  let search_and_display ~default_url =
    match Args.parse_args with
    | Error (#Common.Err.t as e) -> Cli.handle_err @@ `CommonErr e
    | Ok url_str -> (
        let url =
          Common.Urllib.parse
            (match url_str with "" -> default_url | _ as addr -> addr)
            ""
        in
        match
          Backend.get ~url:(Common.Urllib.to_string url) ~host:url.domain
        with
        | Ok ({ Mime.media_type = `Gemini; _ }, body) ->
            if !Args.raw then Cli.handle_text "" body
            else Cli.handle_gemini @@ Gemini.Text.parse body
        | Ok ({ Mime.media_type = `Text txt; _ }, body) ->
            Cli.handle_text txt body
        | Ok ({ Mime.media_type = `Other o; _ }, body) ->
            Cli.handle_other o body
        | Error (#G.Status.err as e) -> Cli.handle_err @@ `GeminiErr e
        | Error (#Common.Err.t as e) -> Cli.handle_err @@ `CommonErr e)
end
