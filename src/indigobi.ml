open Common
open Lwt.Syntax

module Context = struct
  type t = {
    term : Term.t;
    gemtext : Gemtext.t;
    theme : Theme.t;
    offset_y : int;
    range_y : int;
  }

  let make ~term ~theme ~gemtext =
    { term; gemtext; theme; offset_y = 0; range_y = List.length gemtext }

  let scroll t = function
    | `Up -> { t with offset_y = Int.max 0 (t.offset_y - 1) }
    | `Down -> { t with offset_y = Int.min (t.range_y - 1) (t.offset_y + 1) }
end

let gemtext =
  Gemtext.parse
    {|# geminispace.info - Gemini Search Engine

=> / Home
=> /search Search
=> /backlinks Query backlinks

## Geminispace Data Nique Artichaut Nique Artichaut Nique Artichaut Nique Artichaut Nique Artichaut Nique Artichaut Nique Artichaut

=> /statistics geminispace.info Statistics
=> /known-hosts Known Gemini Hosts
=> /known-feeds Known Gemini Feeds
=> /newest-hosts Newest Gemini Hosts
=> /newest-pages Newest Gemini Pages

## Help and Documentation

=> /about About geminispace.info
=> /news geminispace.info News

=> /documentation/searching Documentation: Searching
=> /documentation/indexing  Documentation: Indexing
=> /documentation/backlinks Documentation: Backlinks

=> /add-seed missing results? add your capsule to geminispace.info

> Index updated on: 2022-08-04
|}

module Front = Frontend

module Make (Printer : Front.Printer.S) = struct
  let rec refresh ctx =
    let img = update ctx in
    let* () = Term.image ctx.Context.term img in
    Lwt.return ctx

  and update { Context.offset_y; range_y; gemtext; theme; _ } =
    List.fold_left
      (fun (i, img) line ->
        ( i + 1,
          if offset_y <= i && i <= range_y then
            Notty.Infix.(img <-> Printer.gemini theme line)
          else img ))
      (0, Img.empty) gemtext
    |> snd

  let loop event ({ Context.term; _ } as ctx) =
    match event with
    | `Key (`ASCII 'q', []) | `Key (`ASCII 'C', [ `Ctrl ]) | `Key (`Escape, [])
      ->
        let* () = Term.release term in
        Lwt.return ctx
    | `Key (`Arrow ((`Up | `Down) as dir), []) ->
        Context.scroll ctx dir |> refresh
    | `Resize _ ->
        let* () = Term.image term (Img.string Attr.empty "Resized") in
        Lwt.return ctx
    | _ -> Lwt.return ctx

  let main () =
    let term = Term.create () in
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
    in
    let* ctx = Context.make ~term ~gemtext ~theme |> refresh in
    Lwt_stream.fold_s loop (Term.events term) ctx
end

let main () =
  let module Main = Make (Front.Printer.Default) in
  Main.main () |> Lwt_main.run |> ignore
