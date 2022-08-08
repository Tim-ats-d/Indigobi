open Import
open Lwt.Syntax
module Print = Front.Printer.Make (Config.Theme.Default)
module Widgets = Widgets.Make (Config.Theme.Default)

module HistEntry = struct
  open Sexplib.Conv

  type t = Lib.Url.t = {
    scheme : string;
    domain : string;
    port : int;
    path : string;
    query : string;
  }
  [@@deriving eq, sexp]

  let from_string = Fun.flip Lib.Url.parse ""
  let to_string = Lib.Url.to_string
  let show = to_string
end

let hist = History.create ~fname:"history" (module HistEntry)

let gemtext, headers =
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

> Index updated on: 2022-08-04|}

let gemtext =
  Lwt_list.map_s
    (fun line ->
      let ctx =
        Front.Context.
          { current_url = HistEntry.from_string "todo"; history = hist }
      in
      Print.stylize_gemini ~ctx line)
    gemtext

let main_aux () =
  let waiter, wakener = Lwt.wait () in

  let* lines = gemtext in
  let textbox = new Widgets.textbox lines in
  let mainbox = new LTerm_widget.vbox in
  let view = new LTerm_widget.hbox in
  let toc = new Widgets.toc headers in
  let sep = new LTerm_widget.vline in
  let status = new LTerm_widget.label "Statusbar" in

  mainbox#on_event (function
    | LTerm_event.Key LTerm_key.{ code = Escape; _ } ->
        Lwt.wakeup wakener ();
        true
    | LTerm_event.Key LTerm_key.{ code = Char c; _ }
      when Uchar.equal c @@ Uchar.of_char 't' ->
        if toc#is_enable then (
          toc#disable;
          view#remove sep;
          view#remove toc)
        else (
          toc#enable;
          view#add ~expand:false sep;
          view#add toc);
        true
    | LTerm_event.Key LTerm_key.{ code = Up; _ } ->
        textbox#goup;
        true
    | LTerm_event.Key LTerm_key.{ code = Down; _ } ->
        textbox#godown;
        true
    | _ -> false);

  view#add textbox;
  mainbox#add view;
  mainbox#add ~expand:false (new LTerm_widget.hline);
  mainbox#add ~expand:false status;

  let* term = Lazy.force LTerm.stdout in
  LTerm_widget.run term mainbox waiter

let main () =
  Printexc.record_backtrace true;
  Printexc.print_backtrace stdout;
  Lwt_main.run @@ main_aux ()
