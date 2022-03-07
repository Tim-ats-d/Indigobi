module Color = struct
  open LTerm_style

  let default = default
  let blue = rgb 88 166 255
  let dark_blue = rgb 0 44 170
  let dark_green = rgb 9 121 49
  let dark_grey = rgb 48 54 61
  let dark_red = rgb 177 13 13
  let light_grey = rgb 139 148 158
  let light_red = rgb 211 60 46
  let purple = rgb 156 75 191
  let yellow = rgb 249 214 74
end

module Cfg = struct
  type t = Gemini.Text.line
  type color = LTerm_style.color
  type markup = LTerm_text.markup

  let error_clr = Color.dark_red

  open LTerm_text

  let make_prompt meta =
    let msg = meta ^ " " in
    [ B_fg Color.purple; S msg; E_fg ]

  let rec to_markup =
    let open Gemini.Text in
    function
    | Text txt -> [ B_fg Color.default; S txt; E_fg ]
    | Link { url; name } -> markup_of_link ~url ~name
    | Preformat { text; _ } -> [ B_fg Color.yellow; S text; E_fg ]
    | Heading (`H1, h) ->
        [ B_bold true; B_fg Color.light_red; S "# "; S h; E_fg; E_bold ]
    | Heading (`H2, h) ->
        [ B_bold true; B_fg Color.dark_green; S "## "; S h; E_fg; E_bold ]
    | Heading (`H3, h) ->
        [ B_bold true; B_fg Color.dark_blue; S "### "; S h; E_fg; E_bold ]
    | ListItem item ->
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
        [
          B_fg Color.dark_grey; S " █ "; E_fg; B_fg Color.light_grey; S q; E_fg;
        ]

  and markup_of_link ~url ~name =
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
end
