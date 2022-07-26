module type ABSTRACT = sig
  type style

  val text : style
  val link_arrow : style
  val link_name : style
  val link_url : style
  val visited_link : style
  val preformat : style
  val h1 : style
  val h2 : style
  val h3 : style
  val list_bullet : style
  val list_item : style
  val quote_indent : style
  val quotation : style
  val prompt : style
  val warning : style
  val error : style
end

module type S = ABSTRACT with type style := LTerm_style.t

open Sexplib.Conv

type t = {
  name : string;
  text : style;
  link : link;
  visited_link : style;
  preformat : style;
  h1 : style;
  h2 : style;
  h3 : style;
  quote : quote;
  list_item : list_item;
  meta : meta;
}
[@@deriving sexp]

and link = { arrow : style; lname : style; url : style }
and quote = { indent : style; quotation : style }
and list_item = { bullet : style; item : style }
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
    let r, g, b = (conv 0 2, conv 2 2, conv 4 2) in
    Some (LTerm_style.rgb r g b)
  with Failure _ | Invalid_argument _ -> None

let packed
    {
      text;
      link = { arrow; lname; url };
      visited_link;
      preformat;
      h1;
      h2;
      h3;
      list_item = { bullet; item };
      quote = { indent; quotation };
      meta = { prompt; warning; error };
      _;
    } =
  (module struct
    let text = to_lterm_style text
    let link_arrow = to_lterm_style arrow
    let link_name = to_lterm_style lname
    let link_url = to_lterm_style url
    let visited_link = to_lterm_style visited_link
    let preformat = to_lterm_style preformat
    let h1 = to_lterm_style h1
    let h2 = to_lterm_style h2
    let h3 = to_lterm_style h3
    let list_bullet = to_lterm_style bullet
    let list_item = to_lterm_style item
    let quote_indent = to_lterm_style indent
    let quotation = to_lterm_style quotation
    let prompt = to_lterm_style prompt
    let warning = to_lterm_style warning
    let error = to_lterm_style error
  end : S)

let default =
{|
((name "")
(text ((color ((fg "") (bg ""))) (attr ())))
(link
   ((arrow ((color ((fg "") (bg ""))) (attr (bold))))
      (lname ((color ((fg "") (bg ""))) (attr ())))
      (url ((color ((fg 58a6ff) (bg ""))) (attr (bold underline))))))
(visited_link ((color ((fg 9c4bbf) (bg ""))) (attr ())))
(preformat ((color ((fg f9d64a) (bg ""))) (attr ())))
(h1 ((color ((fg d33c2e) (bg ""))) (attr (bold))))
(h2 ((color ((fg 097931) (bg ""))) (attr ())))
(h3 ((color ((fg 002caa) (bg ""))) (attr ())))
(list_item
   ((bullet ((color ((fg "") (bg ""))) (attr (bold))))
      (item ((color ((fg "") (bg ""))) (attr ())))))
(quote
   ((indent ((color ((fg 30363d) (bg ""))) (attr ())))
      (quotation ((color ((fg 8b949e) (bg ""))) (attr ())))))
(meta
   ((prompt ((color ((fg 9c4bbf) (bg ""))) (attr ())))
      (warning ((color ((fg feaf2f) (bg ""))) (attr ())))
      (error ((color ((fg b1dddd) (bg ""))) (attr ()))))))
|}

module Default = (val Sexplib.Sexp.of_string default |> t_of_sexp |> packed)
