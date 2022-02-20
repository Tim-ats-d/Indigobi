module type S = sig
  val print_err :
    [< `GeminiErr of Gemini.Status.err | `CommonErr of Common.Err.t ] -> unit
end

open Common

module Make (Cfg : Config.S) : S = struct
  let print_err err =
    let msg =
      match err with
      | `GeminiErr g -> "Gemini error: " ^ Gemini.Status.show g
      | `CommonErr err -> "Error: " ^ Common.Err.show err
    in
    let color_msg = LTerm_text.eval [ B_fg Cfg.error; S msg; E_fg ] in
    Lwt_main.run @@ LTerm.eprintls color_msg
end
