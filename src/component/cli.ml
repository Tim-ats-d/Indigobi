module type S = sig
  val handle_text : string -> string -> unit
  val handle_gemini : Gemini.Text.t -> unit
  val handle_other : string -> string -> unit

  val handle_err :
    [< `GeminiErr of Gemini.Status.err | `CommonErr of Common.Err.t ] -> unit
end

open Common

module Make
    (Cfg : Config.S
             with type t := Gemini.Text.line
              and type color = LTerm_style.color
              and type markup = LTerm_text.markup) : S = struct
  let handle_text _ = print_endline

  let handle_gemini lines =
    let term = Lazy.force LTerm.stdout in
    Lwt_main.run
    @@ Lwt_list.iter_s
         Lwt.(
           fun line ->
             term >>= fun term ->
             (try Cfg.to_markup line |> LTerm_text.eval |> LTerm.fprintls term
              with Zed_string.Invalid (_, text) -> LTerm.printl text)
             >>= fun () -> LTerm.flush term)
         lines

  let handle_other _ _ = failwith "todo: non-text format"

  let handle_err err =
    let msg =
      match err with
      | `GeminiErr g -> "Gemini error: " ^ Gemini.Status.show g
      | `CommonErr err -> "Error: " ^ Common.Err.show err
    in
    let color_msg = LTerm_text.eval [ B_fg Cfg.error_clr; S msg; E_fg ] in
    Lwt_main.run
      Lwt.(
        Lazy.force LTerm.stderr >>= fun term ->
        LTerm.fprintls term color_msg >>= fun () -> LTerm.flush term)
end
