module type S = sig
  val stylize_gemini :
    ctx:'a Context.t -> Gemini.Text.line -> LTerm_text.t Lwt.t

  val stylize_prompt : string -> LTerm_text.t
  val stylize_warning : string -> LTerm_text.t
  val stylize_error : string -> LTerm_text.t
end

module Make (Theme : Config.Theme.S) : S = struct
  module Color = Config.Color
  open Lwt.Syntax
  open Common

  let stylize_gemini (type a) ~ctx:{ Context.current_url; history } = function
    | Gemini.Text.Text txt -> Lwt.return @@ LTerm_text.stylise txt Theme.text
    | Link { url; name } ->
        let module HistEntry = (val History.entry history : History.ENTRY
                                  with type t = a)
        in
        let url_obj =
          Urllib.(parse url "" |> replace_path current_url |> to_string)
        in
        let* is_visited =
          History.mem history @@ HistEntry.from_string url_obj
        in
        let url_color = if is_visited then Theme.visited_link else Theme.link in
        Lwt.return
        @@ Array.concat
             [
               LTerm_text.eval [ B_bold true; S "⇒ "; B_underline true ];
               LTerm_text.stylise url url_color;
               LTerm_text.eval
                 [
                   E_underline;
                   S " ";
                   B_fg Color.default;
                   S (Option.value name ~default:"");
                   E_fg;
                   E_bold;
                 ];
             ]
    | Preformat { text; _ } ->
        Lwt.return @@ LTerm_text.stylise text Theme.preformat
    | Heading (`H1, h) -> Lwt.return @@ LTerm_text.stylise h Theme.h1
    | Heading (`H2, h) -> Lwt.return @@ LTerm_text.stylise h Theme.h2
    | Heading (`H3, h) -> Lwt.return @@ LTerm_text.stylise h Theme.h3
    | ListItem item ->
        Lwt.return
        @@ LTerm_text.eval
             [
               S " ";
               B_bold true;
               B_fg Color.default;
               S " ";
               E_fg;
               E_bold;
               S " ";
               S item;
             ]
    | Quote q ->
        Lwt.return
        @@ LTerm_text.eval
             [
               B_fg Color.dark_grey;
               S " █ ";
               E_fg;
               B_fg Color.light_grey;
               S q;
               E_fg;
             ]

  let stylize_error = Fun.flip LTerm_text.stylise Theme.error
  let stylize_prompt meta = LTerm_text.stylise (meta ^ " ") Theme.prompt
  let stylize_warning = Fun.flip LTerm_text.stylise Theme.warning
end
