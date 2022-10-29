open Common

type t = {
  term : Term.t;
  document : document;
  homepage : document;
  theme : Theme.t;
  mode : mode;
  tab : tab * Mime.t;
  offset : int;
  range : int;
  reload : bool;
  args : Cli.t;
}

and document = Gemtext of Gemtext.t | Text of string * string
and mode = Browse
and tab = Home | Page of { address : string }

let doc_len = function
  | Gemtext g -> List.length g
  | Text (t, _) -> (String.split_on_char '\n' t) |> List.length

let mime_of_doc = function
  | Gemtext _ -> Mime.Gemini
  | Text (_, mime) -> Text mime

let make ~term ~theme ~homepage ~tab ~args =
  {
    term;
    document = Gemtext [];
    homepage;
    theme;
    mode = Browse;
    tab = (tab, match tab with Home -> mime_of_doc homepage | _ -> Gemini);
    offset = 0;
    range = 0;
    reload = true;
    args;
  }

let reload reload ctx = { ctx with reload }

let scroll t = function
  | `Up -> { t with offset = Int.max 0 (t.offset - 1) }
  | `Down -> { t with offset = Int.min (t.range - 1) (t.offset + 1) }

let set_home document ctx =
  {
    ctx with
    document;
    mode = Browse;
    tab = (Home, mime_of_doc document);
    offset = 0;
    range = doc_len document;
  }

let set_page document address ctx =
  {
    ctx with
    document;
    mode = Browse;
    tab = (Page { address }, mime_of_doc document);
    offset = 0;
    range = doc_len document;
  }

let set_error err ctx =
  let head, body =
    match err with
    | #Backend.Status.err as e ->
        ("Gemini error", Format.sprintf "%a" Backend.Status.pp e)
    | #Err.err as e -> ("Error", Format.sprintf "%a" Err.pp e)
  in
  let document = Gemtext [ Heading (`H1, head); Text ""; Text body ] in
  { ctx with document; offset = 0; range = doc_len document }

let pp_mode () = function Browse -> "BROWSE"
