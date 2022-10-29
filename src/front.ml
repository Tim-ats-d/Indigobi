open Common
open Frontend

module type S = sig
  val main : unit -> unit
end

module Make (Back : Back.S) (ArgParser : Cli.S) (Printer : Frontend.Printer.S) :
  S = struct
  open Lwt.Syntax
  open Notty.Infix

  let rec refresh ctx =
    let* ctx, img = update ctx in
    let* () = Term.image ctx.Context.term img in
    Lwt.return ctx

  and update ({ Context.mode; reload; tab; _ } as ctx) =
    let* ctx =
      match (mode, tab) with
      | Browse, (Home, _) -> Context.set_home ctx.homepage ctx |> Lwt.return
      | Browse, (Page { address }, _) ->
          if reload then browse ctx address else Lwt.return ctx
    in
    Lwt.return (ctx, mk_status ctx </> mk_view ctx)

  and browse ctx address =
    let url = Url.parse address "" in
    match%lwt Back.cert_from_file "" with
    | Ok c -> (
        match%lwt
          Back.get ~url:(Url.to_string url) ~host:url.domain ~port:url.port
            ~cert:c ctx.args.timeout
        with
        | Ok (_, body) ->
            Context.set_page (Gemtext (Gemtext.parse body)) address ctx
            |> Context.reload false |> Lwt.return
        | Error err -> Context.set_error err ctx |> Lwt.return)
    | Error err -> Context.set_error err ctx |> Lwt.return

  and mk_view { Context.document; offset; range; theme; _ } =
    let print doc p =
      List.fold_left
        (fun (i, img) line ->
          ( i + 1,
            if offset <= i && i <= range then img <-> p theme line else img ))
        (0, Img.empty) doc
      |> snd
    in
    match document with
    | Gemtext gt -> print gt Printer.gemini
    | Text (txt, _mime) ->
        print (String.split_on_char '\n' txt) (fun theme ->
            Img.string theme.text)

  and mk_status { Context.mode; offset; range; tab = address, mime; term; _ } =
    let mode = Format.sprintf "%a" Context.pp_mode mode in
    let address =
      match address with Home -> "Home" | Page { address } -> address
    in
    let back = Attr.(fg black ++ bg white) in
    let sep = Img.string back " | " in
    let mode = mode |> Img.string back in
    let addr = Img.string back address in
    let mime = Format.sprintf "%a" Mime.pp mime |> Img.string back in
    let progress = Printf.sprintf "%i/%i" offset range |> Img.string back in
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
    | `Mouse (`Press (`Scroll dir), _, _)
    | `Key (`Arrow ((`Up | `Down) as dir), []) ->
        Context.scroll ctx dir |> refresh
    | `Key (`Enter, _) -> Context.scroll ctx `Down |> refresh
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

  let homepage = Context.Gemtext [ Heading (`H1, "Home") ]

  let main () =
    Lwt_main.run
    @@
    match ArgParser.parse () with
    | Ok args ->
        let term = Term.create () in
        let tab =
          Option.fold args.address ~none:Context.Home ~some:(fun address ->
              Page { address })
        in
        let* ctx = Context.make ~term ~theme ~homepage ~tab ~args |> refresh in
        let* _ = Lwt_stream.fold_s loop (Term.events term) ctx in
        Lwt.return_unit
    | Error err -> Format.sprintf "%a" Err.pp (err :> Err.err) |> Lwt_io.printl
end
