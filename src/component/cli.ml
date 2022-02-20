module type S = sig
  val print_err :
    [< `GeminiErr of Gemini.Status.err | `CommonErr of Common.Err.t ] -> unit
end

let red = LTerm_style.rgb 177 13 13

module Default : S = struct
  let print_err err =
    let msg =
      match err with
      | `GeminiErr g -> "Gemini error: " ^ Gemini.Status.show g
      | `CommonErr err -> "Error: " ^ Common.Err.show err
    in
    let color_msg = LTerm_text.eval [ B_fg red; S msg; E_fg ] in
    Lwt_main.run @@ LTerm.eprintls color_msg
end
