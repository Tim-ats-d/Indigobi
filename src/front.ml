open Common
open Frontend

module type S = sig
  val main : unit -> unit
end

module Make (Back : Back.S) (Printer : Frontend.Printer.S) : S = struct
  open Lwt.Syntax
  open Notty.Infix

  let rec refresh ctx =
    let* ctx, img = update ctx in
    let* () = Term.image ctx.Draw_ctx.term img in
    Lwt.return ctx

  and update ({ Draw_ctx.mode; reload; _ } as ctx) =
    let* ctx =
      match mode with
      | Browse { address; _ } ->
          if reload then browse ctx address else Lwt.return ctx
    in

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
            Draw_ctx.set_text (Gemtext.parse body) address (Some mime) ctx
            |> Draw_ctx.reload false |> Lwt.return
        | Error err -> Draw_ctx.set_error err ctx |> Lwt.return)
    | Error err -> Draw_ctx.set_error err ctx |> Lwt.return

  and mk_view { Draw_ctx.text; offset; range; theme; _ } =
    List.fold_left
      (fun (i, img) line ->
        ( i + 1,
          if offset <= i && i <= range then img <-> Printer.gemini theme line
          else img ))
      (0, Img.empty) text
    |> snd

  and mk_status { Draw_ctx.mode; offset; range; term; _ } =
    let address, mime =
      match mode with Browse { address; mime } -> (address, mime)
    in
    let back = Attr.(fg black ++ bg white) in
    let sep = Img.string back " | " in
    let mode = Format.sprintf "%a" Draw_ctx.pp_mode mode |> Img.string back in
    let addr = Img.string back address in
    let mime =
      Option.fold mime ~none:Img.empty ~some:(fun m ->
          Format.sprintf "%a" Mime.pp m |> Img.string back <|> sep)
    in
    let progress = Printf.sprintf "%i/%i" offset range |> Img.string back in
    let col, row = Term.size term in
    let bar = Img.uchar back (Uchar.of_char ' ') col 1 in
    let left = mode <|> sep <|> addr in
    let right = mime <|> progress in
    left
    </> Img.hpad (Img.width bar - Img.width right) 0 right
    </> bar
    |> Img.vpad (row - 1) 0

  let loop event ({ Draw_ctx.term; _ } as ctx) =
    match event with
    | `Key (`ASCII 'q', []) | `Key (`ASCII 'C', [ `Ctrl ]) | `Key (`Escape, [])
      ->
        let* () = Term.release term in
        Lwt.return ctx
    | `Resize _ -> refresh ctx
    | `Key (`Arrow ((`Up | `Down) as dir), []) ->
        Draw_ctx.scroll ctx dir |> refresh
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
      Draw_ctx.make ~term ~theme ~mode:(Draw_ctx.browse address None) |> refresh
    in
    Lwt_stream.fold_s loop (Term.events term) ctx

  let main () = main_aux () |> Lwt_main.run |> ignore
end
