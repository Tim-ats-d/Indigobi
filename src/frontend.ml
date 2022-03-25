open Import

module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make (Cli : Cli.S) (Backend : Backend.S) (ArgParser : Args.S) : S =
struct
  let handle_no_url url_opt =
    match url_opt with
    | None ->
        Lwt_main.run @@ Cli.handle_err @@ `CommonErr `NoUrlProvided;
        exit 1
    | Some addr -> addr

  let launch () =
    let args = ArgParser.parse () in
    let url = Common.Urllib.parse (handle_no_url args.Args.address) "" in
    match Backend.get ~url:(Common.Urllib.to_string url) ~host:url.domain with
    | Ok ({ Mime.media_type = `Gemini; _ }, body) ->
        if args.Args.raw then Cli.handle_text "" body
        else Cli.handle_gemini @@ Gemini.Text.parse body
    | Ok ({ Mime.media_type = `Text txt; _ }, body) -> Cli.handle_text txt body
    | Ok ({ Mime.media_type = `Other o; _ }, body) -> Cli.handle_other o body
    | Error (#G.Status.err as e) -> Cli.handle_err @@ `GeminiErr e
    | Error (#Common.Err.t as e) -> Cli.handle_err @@ `CommonErr e
end
