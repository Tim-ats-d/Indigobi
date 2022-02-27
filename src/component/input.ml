open Common

module type S = sig
  val input : string -> string
  val sensitive : string -> string
end

module Make (Cfg : Config.S) = struct
  let input meta =
    Cfg.make_prompt meta |> LTerm_text.eval |> LTerm.prints |> Lwt_main.run;
    try Urllib.encode @@ input_line stdin with End_of_file -> exit 1

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
    let ( >>= ) = Lwt.( >>= ) in
    let launch () =
      Lazy.force LTerm.stdout >>= fun term ->
      LTerm.flush term >>= fun () ->
      let prompt = LTerm_text.eval @@ Cfg.make_prompt meta in
      (new hidden_read ~prompt term)#run >>= fun input ->
      Zed_string.to_utf8 input |> Urllib.encode |> Lwt.return
    in
    Lwt_main.run @@ launch ()
end
