open Import

module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make (Backend : Backend.S) (PPrint : Pprint.S) (ArgParser : Args.S) : S =
struct
  let launch () =
    let args = ArgParser.parse () in
    let url =
      match args.Args.address with
      | None ->
          Lwt_main.run @@ PPrint.handle_err @@ `CommonErr `NoUrlProvided;
          exit 1
      | Some adrr -> Common.Urllib.parse adrr ""
    in
    match
      Backend.get
        ~url:(Common.Urllib.to_string url)
        ~host:url.domain ~port:url.port ~cert:args.Args.certificate
    with
    | Ok ({ Mime.media_type = `Gemini; _ }, body) ->
        if args.Args.raw then PPrint.handle_text "" body
        else PPrint.handle_gemini @@ Gemini.Text.parse body
    | Ok ({ Mime.media_type = `Text txt; _ }, body) ->
        PPrint.handle_text txt body
    | Ok ({ Mime.media_type = `Other o; _ }, body) -> PPrint.handle_other o body
    | Error (#G.Status.err as e) -> PPrint.handle_err @@ `GeminiErr e
    | Error (#Common.Err.t as e) -> PPrint.handle_err @@ `CommonErr e
end
