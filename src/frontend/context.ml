open Common

type t = {
  term : Term.t;
  document : document;
  homepage : document;
  theme : Theme.t;
  mode : mode;
  tab : tab * Mime.t;
  input : string;
  offset : int;
  range : int;
  reload : bool;
  history : tab Zipper.t;
  args : Cli.t;
}

and document = Gemtext of Gemtext.t | Text of string * string
and mode = Browse | Input
and tab = Home | Page of { address : string }

let doc_len = function
  | Gemtext g -> List.length g
  | Text (t, _) -> String.split_on_char '\n' t |> List.length

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
    input = (match tab with Home -> "" | Page { address } -> address);
    range = 0;
    reload = true;
    history = Zipper.make tab;
    args;
  }

let set_mode mode ctx = { ctx with mode }
let set_history history ctx = { ctx with history }

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
    input = address;
    offset = 0;
    range = doc_len document;
  }

let set_error err ~address ctx =
  let head, body =
    match err with
    | #Backend.Status.err as e ->
        ("Gemini error", Format.sprintf "%a" Backend.Status.pp e)
    | #Err.err as e -> ("Error", Format.sprintf "%a" Err.pp e)
  in
  let document = Gemtext [ Heading (`H1, head); Text ""; Text body ] in
  {
    ctx with
    document;
    tab = (Page { address }, Gemini);
    offset = 0;
    range = doc_len document;
  }

let reload reload ctx = { ctx with reload }

let scroll ctx = function
  | `Up -> { ctx with offset = Int.max 0 (ctx.offset - 1) }
  | `Down -> { ctx with offset = Int.min (ctx.range - 1) (ctx.offset + 1) }

let delete_last ctx =
  if String.length ctx.input = 0 then ctx
  else { ctx with input = String.sub ctx.input 0 (String.length ctx.input - 1) }

let toggle ctx state ~default =
  {
    ctx with
    mode = (match ctx.mode with s when s = state -> default | _ -> state);
  }

let pp_mode () = function Browse -> "BROWSE" | Input -> "INPUT"
