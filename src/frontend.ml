open Import

module type S = sig
  val search_and_display : url:string -> host:string -> unit Lwt.t
end

module Make (Cli : Cli.S) (Backend : Backend.S) : S = struct
  let search_and_display ~url ~host =
    match Backend.get ~url ~host with
    | Ok ({ Mime.media_type = `Gemini; _ }, body) ->
        Cli.handle_gemini @@ Gemini.Text.parse body
    | Ok ({ Mime.media_type = `Text txt; _ }, body) -> Cli.handle_text txt body
    | Ok ({ Mime.media_type = `Other o; _ }, body) -> Cli.handle_other o body
    | Error (#G.Status.err as e) -> Cli.handle_err @@ `GeminiErr e
    | Error (#Common.Err.t as e) -> Cli.handle_err @@ `CommonErr e
end
