open Common

type t = {
  term : Term.t;
  text : Gemtext.t;
  theme : Theme.t;
  mode : mode;
  offset : int;
  range : int;
  reload : bool;
}

and mode = Browse of { address : string; mime : Mime.t option }

let browse address mime = Browse { address; mime }

let make ~term ~theme ~mode =
  { term; text = []; theme; mode; offset = 0; range = 0; reload = true }

let reload reload ctx = { ctx with reload }

let set_text text address mime ctx =
  {
    ctx with
    text;
    mode = Browse { address; mime };
    offset = 0;
    range = List.length text;
  }

let set_error err ctx =
  let head, body =
    match err with
    | #Backend.Status.err as e ->
        ("Gemini error", Format.sprintf "%a" Backend.Status.pp e)
    | #Err.err as e -> ("Error", Format.sprintf "%a" Err.pp e)
  in
  let text = [ Gemtext.Heading (`H1, head); Text ""; Text body ] in
  { ctx with text; offset = 0; range = List.length text }

let pp_mode () = function Browse _ -> "BROWSE"

let scroll t = function
  | `Up -> { t with offset = Int.max 0 (t.offset - 1) }
  | `Down -> { t with offset = Int.min (t.range - 1) (t.offset + 1) }
