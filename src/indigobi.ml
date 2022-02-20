module Component = Component
module Common = Common
module Gemini = Gemini

module Color = struct
  open LTerm_style

  let default = default
  let blue = rgb 88 166 255
  let dark_grey = rgb 48 54 61
  let light_grey = rgb 139 148 158
  let purple = rgb 156 75 191
  let red = rgb 177 13 13
end

module Cfg : Common.Config.S = struct
  open LTerm_text

  let error_clr = Color.red

  let make_prompt meta =
    let msg = meta ^ " " in
    [ B_fg Color.purple; S msg; E_fg ]

  let fmt_text txt = [ B_fg Color.default; S txt; E_fg ]

  let fmt_link ~url ~name =
    [
      B_bold true;
      S "⇒ ";
      B_underline true;
      B_fg Color.blue;
      S url;
      E_fg;
      E_underline;
      E_bold;
      S " ";
      B_fg Color.default;
      S (Option.value name ~default:"");
      E_fg;
    ]

  let fmt_preformat text _alt = [ B_reverse true; S text; E_reverse ]
  let fmt_h1 h = [ B_bold true; B_fg Color.default; S "# "; S h; E_fg; E_bold ]
  let fmt_h2 h = [ B_bold true; B_fg Color.default; S "## "; S h; E_fg; E_bold ]

  let fmt_h3 h =
    [ B_bold true; B_fg Color.default; S "### "; S h; E_fg; E_bold ]

  let fmt_list_item item =
    [
      S " "; B_bold true; B_fg Color.default; S " "; E_fg; E_bold; S " "; S item;
    ]

  let fmt_quote q =
    [ B_fg Color.dark_grey; S " █ "; E_fg; B_fg Color.light_grey; S q; E_fg ]
end

let main () =
  let open Component in
  let module Back = Backend.Make (Input.Make (Cfg)) (Requester.Default) in
  let module Front = Frontend.Make (Cli.Make (Cfg)) (Back) in
  Front.get ~url:"gemini://geminispace.info/search" ~host:"geminispace.info"
