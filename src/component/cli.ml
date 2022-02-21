module type S = sig
  val print_text : string -> string -> unit
  val print_gemini : Gemini.Text.t -> unit
  val print_other : string -> string -> unit

  val print_err :
    [< `GeminiErr of Gemini.Status.err | `CommonErr of Common.Err.t ] -> unit
end

open Common

module Make (Cfg : Config.S) : S = struct
  let print_text _ = print_endline

  let rec print_gemini lines =
    Lwt_main.run
    @@ Lwt_list.iter_p
         (fun line -> markup_of_line line |> LTerm_text.eval |> LTerm.printls)
         lines

  and markup_of_line =
    let open Gemini.Text in
    function
    | Text t -> Cfg.fmt_text t
    | Link { url; name } -> Cfg.fmt_link ~url ~name
    | Preformat { text; alt } -> Cfg.fmt_preformat text alt
    | Heading (`H1, h) -> Cfg.fmt_h1 h
    | Heading (`H2, h) -> Cfg.fmt_h2 h
    | Heading (`H3, h) -> Cfg.fmt_h3 h
    | ListItem i -> Cfg.fmt_list_item i
    | Quote q -> Cfg.fmt_quote q

  let print_other _ _ = failwith "todo: non-text format"

  let print_err err =
    let msg =
      match err with
      | `GeminiErr g -> "Gemini error: " ^ Gemini.Status.show g
      | `CommonErr err -> "Error: " ^ Common.Err.show err
    in
    let color_msg = LTerm_text.eval [ B_fg Cfg.error_clr; S msg; E_fg ] in
    Lwt_main.run @@ LTerm.eprintls color_msg
end
