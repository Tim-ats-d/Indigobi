module type S = sig
  val stylize_gemini : Gemini.Text.line -> LTerm_text.t
  val stylize_prompt : string -> LTerm_text.t
  val stylize_warning : string -> LTerm_text.t
  val stylize_error : string -> LTerm_text.t
end

module Make (Theme : Config.Theme.S) : S = struct
  module Color = Config.Color

  let stylize_gemini = function
    | Gemini.Text.Text txt -> LTerm_text.stylise txt Theme.text
    | Link { url; name } ->
        LTerm_text.eval
          [
            B_bold true;
            S "⇒ ";
            B_underline true;
            B_fg Color.blue;
            S url;
            E_fg;
            E_underline;
            S " ";
            B_fg Color.default;
            S (Option.value name ~default:"");
            E_fg;
            E_bold;
          ]
    | Preformat { text; _ } -> LTerm_text.stylise text Theme.preformat
    | Heading (`H1, h) -> LTerm_text.stylise h Theme.h1
    | Heading (`H2, h) -> LTerm_text.stylise h Theme.h2
    | Heading (`H3, h) -> LTerm_text.stylise h Theme.h3
    | ListItem item ->
        LTerm_text.eval
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
        LTerm_text.eval
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
