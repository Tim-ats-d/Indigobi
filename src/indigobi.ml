module Attr = Notty.A
module Img = Notty.I
module Term = Notty_lwt.Term
open Lwt.Syntax

module Context = struct
  type t = {
    term : Term.t;
    gemtext : Gemini.Gemtext.t;
    offset_y : int;
    range_y : int;
  }

  let make ~term ~gemtext =
    { term; gemtext; offset_y = 0; range_y = List.length gemtext }

  let scroll t = function
    | `Up -> { t with offset_y = Int.max 0 (t.offset_y - 1) }
    | `Down -> { t with offset_y = Int.min (t.range_y / 2) (t.offset_y + 1) }
end

let gemtext =
  Gemini.Gemtext.parse
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

let rec refresh ctx =
  let img = update ctx in
  let* () = Term.image ctx.Context.term img in
  Lwt.return ctx

and update { Context.offset_y; range_y; gemtext; _ } =
  List.fold_left
    (fun (i, img) line ->
      ( i + 1,
        if offset_y <= i && i <= range_y then
          let open Notty.Infix in
          img
          <->
          match line with
          | Gemini.Gemtext.Text t -> Img.string Attr.empty t
          | Link { url; name } ->
              Img.string Attr.empty "=> "
              <|> Img.string Attr.(st bold ++ st underline ++ fg lightblue) url
              <|> Option.fold name ~none:Img.empty ~some:(fun name ->
                      Img.string Attr.empty " "
                      <|> Img.string Attr.(st bold) name)
          | Preformat { text; _ } -> Img.string Attr.(fg (gray 4)) text
          | Heading (`H1, head) -> Img.string Attr.(fg red) head
          | Heading (`H2, head) -> Img.string Attr.(fg green) head
          | Heading (`H3, head) -> Img.string Attr.(fg blue) head
          | ListItem item -> Img.string Attr.empty ("* " ^ item)
          | Quote quote -> Img.string Attr.(fg (gray 12)) ("█ " ^ quote)
        else img ))
    (0, Img.empty) gemtext
  |> snd

let loop event ({ Context.term; _ } as ctx) =
  match event with
  | `Key (`ASCII 'q', []) | `Key (`ASCII 'C', [ `Ctrl ]) | `Key (`Escape, []) ->
      let* () = Term.release term in
      Lwt.return ctx
  | `Key (`Arrow ((`Up | `Down) as dir), []) ->
      Context.scroll ctx dir |> refresh
  | `Resize _ ->
      let* () = Term.image term (Img.string Attr.empty "Resized") in
      Lwt.return ctx
  | _ -> Lwt.return ctx

let mainloop () =
  let term = Term.create () in
  let* ctx = Context.make ~term ~gemtext |> refresh in
  Lwt_stream.fold_s loop (Term.events term) ctx

let main () = mainloop () |> Lwt_main.run |> ignore
