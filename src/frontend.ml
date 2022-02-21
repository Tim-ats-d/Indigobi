open Import

module type S = sig
  val get : url:string -> host:string -> unit
end

module Make (Cli : Cli.S) (Backend : Backend.S) = struct
  let get ~url ~host =
    match Backend.get ~url ~host with
    | Ok ({ Mime.media_type = `Gemini; _ }, body) ->
        Cli.print_gemini @@ Gemini.Text.parse body
    | Ok ({ Mime.media_type = `Text txt; _ }, body) -> Cli.print_text txt body
    | Ok ({ Mime.media_type = `Other o; _ }, body) -> Cli.print_other o body
    | Error (#G.Status.err as e) -> Cli.print_err @@ `GeminiErr e
    | Error (#Common.Err.t as e) -> Cli.print_err @@ `CommonErr e
end
