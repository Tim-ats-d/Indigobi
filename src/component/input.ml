module type S = sig
  val input : string -> string
  val sensitive : string -> string
end

let purple = LTerm_style.rgb 156 75 191

let make_prompt meta =
  let msg = Printf.sprintf "%s: " meta in
  LTerm_text.eval [ B_fg purple; S msg; E_fg ]

module Default : S = struct
  let input meta =
    make_prompt meta |> LTerm.prints |> Lwt_main.run;
    Common.Urllib.encode @@ input_line stdin

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
    let launch () =
      let ( >>= ) = Lwt.( >>= ) in
      LTerm_inputrc.load () >>= fun () ->
      Lazy.force LTerm.stdout >>= fun term ->
      LTerm.flush term >>= fun () ->
      let prompt = make_prompt meta in
      (new hidden_read ~prompt term)#run >>= fun input ->
      Lwt.return @@ Zed_string.to_utf8 input
    in
    Lwt_main.run @@ launch ()
end
