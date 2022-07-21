module type S = sig
  val stylize_gemini :
    history:string Common.History.t -> Gemini.Text.line -> LTerm_text.t Lwt.t

  val stylize_prompt : string -> LTerm_text.t
  val stylize_warning : string -> LTerm_text.t
  val stylize_error : string -> LTerm_text.t
end

module Make (Theme : Config.Theme.S) : S = struct
  module Color = Config.Color
  open Lwt.Syntax

  let stylize_gemini ~history = function
    | Gemini.Text.Text txt -> Lwt.return @@ LTerm_text.stylise txt Theme.text
    | Link { url; name } ->
        let* urls = Common.History.get history in
        let is_visited = List.mem url urls in
        let colored_url =
          LTerm_text.stylise url
          @@ if is_visited then Theme.visited_link else Theme.link
        in
        Lwt.return
        @@ Array.concat
             [
               LTerm_text.eval [ B_bold true; S "⇒ "; B_underline true ];
               colored_url;
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
