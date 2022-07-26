module type ABSTRACT = sig
  type style

  val text : style
  val link : style
  val visited_link : style
  val preformat : style
  val h1 : style
  val h2 : style
  val h3 : style
  val prompt : style
  val warning : style
  val error : style
end

module type S = ABSTRACT with type style := LTerm_style.t

open Sexplib.Conv

type t = {
  name : string;
  text : style;
  link : style;
  visited_link : style;
  preformat : style;
  h1 : style;
  h2 : style;
  h3 : style;
  meta : meta;
}
[@@deriving sexp]

and meta = { prompt : style; warning : style; error : style }
and style = { color : color; attr : attr list }
and color = { fg : string; bg : string }
and attr = [ `bold | `underline | `blink | `reverse ]

let rec to_lterm_style { color = { fg; bg }; attr } =
  {
    LTerm_style.bold = Some (attr_is_set attr `bold);
    underline = Some (attr_is_set attr `underline);
    blink = Some (attr_is_set attr `blink);
    reverse = Some (attr_is_set attr `reverse);
    foreground = lterm_color_of_clr fg;
    background = lterm_color_of_clr bg;
  }

and attr_is_set = Fun.flip List.mem

and lterm_color_of_clr clr =
  try
    let conv i j = String.sub clr i j |> String.cat "0x" |> int_of_string in
    let r, g, b = (conv 0 2, conv 2 4, conv 4 6) in
    Some (LTerm_style.rgb r g b)
  with Failure _ | Invalid_argument _ -> None

let packed
    {
      text;
      link;
      visited_link;
      preformat;
      h1;
      h2;
      h3;
      meta = { prompt; warning; error };
      _;
    } =
  (module struct
    let text = to_lterm_style text
    let link = to_lterm_style link
    let visited_link = to_lterm_style visited_link
    let preformat = to_lterm_style preformat
    let h1 = to_lterm_style h1
    let h2 = to_lterm_style h2
    let h3 = to_lterm_style h3
    let prompt = to_lterm_style prompt
    let warning = to_lterm_style warning
    let error = to_lterm_style error
  end : S)

let default =
  {|
((name "")
(text ((color ((fg "") (bg ""))) (attr ())))
(link ((color ((fg 58a6ff) (bg ""))) (attr ())))
(visited_link ((color ((fg 9c4bbf) (bg ""))) (attr ())))
(preformat ((color ((fg f9d64a) (bg ""))) (attr ())))
(h1 ((color ((fg d33c2e) (bg ""))) (attr (bold))))
(h2 ((color ((fg 97931) (bg ""))) (attr ())))
(h3 ((color ((fg 002caa) (bg ""))) (attr ())))
(meta
   ((prompt ((color ((fg 9c4bbf) (bg ""))) (attr ())))
      (warning ((color ((fg feaf2f) (bg ""))) (attr ())))
      (error ((color ((fg b1dddd) (bg ""))) (attr ()))))))
|}

module Default = (val Sexplib.Sexp.of_string default |> t_of_sexp |> packed)
