open Common

module type S = sig
  val main : unit -> unit
end

module Context = struct
  type t = {
    term : Term.t;
    text : Gemtext.t;
    theme : Theme.t;
    mode : mode;
    offset : int;
    range : int;
    reload : bool;
  }

  and mode =
    | Browse of { address : string; mime : Backend.Mime.t }
    | Error of [ Backend.Status.err | Err.err ]

  let browse address mime = Browse { address; mime }

  let make ~term ~theme ~mode =
    { term; text = []; theme; mode; offset = 0; range = 0; reload = true }

  let reload ctx reload = { ctx with reload }

  let set_text ctx text address mime =
    {
      ctx with
      text;
      mode = Browse { address; mime };
      offset = 0;
      range = List.length text;
    }

  let set_error ctx err =
    let head, body =
      match err with
      | #Backend.Status.err as e ->
          ("Gemini error", Format.sprintf "%a" Backend.Status.pp e)
      | #Err.err as e -> ("Error", Format.sprintf "%a" Err.pp e)
    in
    let text = [ Gemtext.Heading (`H1, head); Text ""; Text body ] in
    { ctx with text; mode = Error err; offset = 0; range = List.length text }

  let pp_mode () = function Browse _ -> "BROWSE" | Error _ -> "ERROR"

  let scroll t = function
    | `Up -> { t with offset = Int.max 0 (t.offset - 1) }
    | `Down -> { t with offset = Int.min (t.range - 1) (t.offset + 1) }
end

module Make (Back : Back.S) (Printer : Frontend.Printer.S) : S = struct
  open Lwt.Syntax
  open Notty.Infix

  let rec refresh ctx =
    let* ctx, img = update ctx in
    let* () = Term.image ctx.Context.term img in
    Lwt.return ctx

  and update ({ Context.mode; reload; _ } as ctx) =
    match mode with
    | Browse { address; _ } when reload -> browse ctx address
    | Browse _ -> Lwt.return (ctx, mk_status ctx </> mk_view ctx)
    | Error err ->
        let ctx = Context.set_error ctx err |> Fun.flip Context.reload false in
        Lwt.return (ctx, mk_status ctx </> mk_view ctx)

  and browse ctx address =
    let url = Url.parse address "" in
    match%lwt Back.cert_from_file "" with
    | Ok c -> (
        match%lwt
          Back.get ~url:(Url.to_string url) ~host:url.domain ~port:url.port
            ~cert:c 5.0
        with
        | Ok (mime, body) ->
            let ctx =
              Context.set_text ctx (Gemtext.parse body) address mime
              |> Fun.flip Context.reload false
            in
            Lwt.return (ctx, mk_status ctx </> mk_view ctx)
        | Error err ->
            let ctx = Context.set_error ctx err in
            Lwt.return (ctx, mk_status ctx </> mk_view ctx))
    | Error err ->
        let ctx = Context.set_error ctx err in
        Lwt.return (ctx, mk_status ctx </> mk_view ctx)

  and mk_view { Context.text; offset; range; theme; _ } =
    List.fold_left
      (fun (i, img) line ->
        ( i + 1,
          if offset <= i && i <= range then img <-> Printer.gemini theme line
          else img ))
      (0, Img.empty) text
    |> snd

  and mk_status { Context.mode; offset; range; term; _ } =
    let address, mime =
      match mode with
      | Browse { address; mime } -> (address, mime)
      | Error _ -> ("Error", Gemini)
    in
    let back = Attr.(fg black ++ bg white) in
    let mode = Format.sprintf "%a" Context.pp_mode mode |> Img.string back in
    let addr = Img.string back address in
    let mime = Format.sprintf "%a" Backend.Mime.pp mime |> Img.string back in
    let progress = Printf.sprintf "%i/%i" offset range |> Img.string back in
    let sep = Img.string back " | " in
    let col, row = Term.size term in
    let bar = Img.uchar back (Uchar.of_char ' ') col 1 in
    let left = mode <|> sep <|> addr in
    let right = mime <|> sep <|> progress in
    left
    </> Img.hpad (Img.width bar - Img.width right) 0 right
    </> bar
    |> Img.vpad (row - 1) 0

  let loop event ({ Context.term; _ } as ctx) =
    match event with
    | `Key (`ASCII 'q', []) | `Key (`ASCII 'C', [ `Ctrl ]) | `Key (`Escape, [])
      ->
        let* () = Term.release term in
        Lwt.return ctx
    | `Resize _ -> refresh ctx
    | `Key (`Arrow ((`Up | `Down) as dir), []) ->
        Context.scroll ctx dir |> refresh
    | _ -> Lwt.return ctx

  let theme =
    Attr.
      {
        Theme.text = empty;
        link = st bold ++ fg blue;
        link_name = st bold;
        preformat = fg (gray 4);
        h1 = fg red;
        h2 = fg green;
        h3 = fg blue;
        item = empty;
        quote = fg (gray 12);
      }

  let main_aux () =
    let term = Term.create () in
    let address = try Sys.argv.(1) with Invalid_argument _ -> "" in
    let* ctx =
      Context.make ~term ~theme ~mode:(Context.browse address (Other "Empty"))
      |> refresh
    in
    Lwt_stream.fold_s loop (Term.events term) ctx

  let main () = main_aux () |> Lwt_main.run |> ignore
end
