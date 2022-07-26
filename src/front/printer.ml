module type S = sig
  val stylize_gemini :
    ctx:'a Context.t -> Gemini.Gemtext.line -> LTerm_text.t Lwt.t

  val stylize_prompt : string -> LTerm_text.t
  val stylize_warning : string -> LTerm_text.t
  val stylize_error : string -> LTerm_text.t
end

module Make (Theme : Config.Theme.S) : S = struct
  open Common
  open Lwt.Syntax

  let stylize_gemini (type a) ~ctx:{ Context.current_url; history } =
    let lwt_stylize str style = Lwt.return @@ LTerm_text.stylise str style in
    function
    | Gemini.Gemtext.Text txt -> lwt_stylize txt Theme.text
    | Link { url; name } ->
        let module HistEntry = (val History.entry history : History.ENTRY
                                  with type t = a)
        in
        let url_obj =
          Lib.Url.(parse url "" |> replace_path current_url |> to_string)
        in
        let* is_visited =
          History.mem history @@ HistEntry.from_string url_obj
        in
        let url_color = if is_visited then Theme.visited_link else Theme.link_url in
        Lwt.return
        @@ Array.concat
             [
               LTerm_text.stylise " ⇒ " Theme.link_arrow;
               LTerm_text.stylise url url_color;
               LTerm_text.eval [ S " " ];
               LTerm_text.stylise
                 (Option.value name ~default:"")
                 Theme.link_name;
             ]
    | Preformat { text; _ } -> lwt_stylize text Theme.preformat
    | Heading (`H1, h) -> lwt_stylize h Theme.h1
    | Heading (`H2, h) -> lwt_stylize h Theme.h2
    | Heading (`H3, h) -> lwt_stylize h Theme.h3
    | ListItem item ->
        Lwt.return
        @@ Array.append
             (LTerm_text.stylise " - " Theme.list_bullet)
             (LTerm_text.stylise item Theme.list_item)
    | Quote q ->
        Lwt.return
        @@ Array.append
             (LTerm_text.stylise " █ " Theme.quote_indent)
             (LTerm_text.stylise q Theme.quotation)

  let stylize_error = Fun.flip LTerm_text.stylise Theme.error
  let stylize_prompt meta = LTerm_text.stylise (meta ^ " ") Theme.prompt
  let stylize_warning = Fun.flip LTerm_text.stylise Theme.warning
end
