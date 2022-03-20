open Common

module type S = sig
  val input : string -> string Lwt.t
  val sensitive : string -> string Lwt.t
end

module Make
    (Cfg : Config.S
             with type color = LTerm_style.color
              and type markup = LTerm_text.markup) =
struct
  open Lwt.Syntax

  let input meta =
    let* term = Lazy.force LTerm.stdout in
    let* () = Cfg.make_prompt meta |> LTerm_text.eval |> LTerm.fprints term in
    let* () = LTerm.flush term in
    let user_input =
      Zed_string.to_utf8 @@ (new LTerm_read_line.read_line ())#eval
    in
    let* () = LTerm.clear_line_prev term in
    Lwt.return @@ user_input

  class hidden_read ~prompt term =
    object (self)
      inherit LTerm_read_line.read_password () as super
      inherit [Zed_string.t] LTerm_read_line.term term
      initializer prompt |> Lwt_react.S.const |> self#set_prompt

      method! send_action =
        function
        | LTerm_read_line.Break -> () | action -> super#send_action action
    end

  let sensitive meta =
    let* term = Lazy.force LTerm.stdout in
    let prompt = LTerm_text.eval @@ Cfg.make_prompt meta in
    let* input = (new hidden_read ~prompt term)#run in
    let* () = LTerm.flush term in
    let user_input = Zed_string.to_utf8 input |> Urllib.encode in
    let* () = LTerm.clear_line_prev term in
    Lwt.return user_input
end
