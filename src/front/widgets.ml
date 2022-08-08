module Make (Theme : Config.Theme.S) = struct
  class textbox (lines : LTerm_text.t list) =
    object (self)
      inherit LTerm_widget.t "textbox"
      val vscroll = new LTerm_widget.scrollable
      initializer self#init_scroll

      method init_scroll =
        vscroll#set_range (List.length lines);
        vscroll#on_offset_change (fun _ -> self#queue_draw)

      (* TODO: better wrapping *)
      method wrap_lines cols =
        let rec wrap acc x =
          let len = Array.length x in
          if len > cols then
            let inserted_line = Array.sub x cols (len - cols) in
            let line = Array.sub x 0 cols in
            wrap (line :: acc) inserted_line
          else List.rev (x :: acc)
        in
        List.fold_left (fun acc line -> acc @ wrap [] line) [] lines

      method goup = vscroll#set_offset (vscroll#offset - 1)
      method godown = vscroll#set_offset (vscroll#offset + 1)

      method! draw ctx _focused =
        let { LTerm_geom.rows; cols } = LTerm_draw.size ctx in
        let wrapped_lines = self#wrap_lines cols in
        for row = 0 to rows - 1 do
          match List.nth_opt wrapped_lines (row + vscroll#offset) with
          | None -> ()
          | Some line -> LTerm_draw.draw_styled ctx row 0 line
        done
    end

  class toc (headers : Gemini.Gemtext.headers) =
    object (self)
      inherit LTerm_widget.t "toc"
      val mutable state = false
      method is_enable = state
      method enable = state <- true
      method disable = state <- false

      method compute =
        let prettify (header, text) =
          let lvl, theme =
            match header with
            | Gemini.Gemtext.H1 -> (0, Theme.h1)
            | H2 -> (2, Theme.h2)
            | H3 -> (4, Theme.h3)
          in
          let prefix = String.make lvl ' ' in
          Array.append
            (LTerm_text.of_utf8 prefix)
            (LTerm_text.stylise text
               {
                 LTerm_style.none with
                 foreground = theme.foreground;
                 background = theme.background;
               })
        in
        List.fold_left (fun acc header -> prettify header :: acc) [] headers

      method! draw ctx _focused =
        let { LTerm_geom.rows; _ } = LTerm_draw.size ctx in
        let lines = self#compute in
        for row = 0 to rows - 1 do
          match List.nth_opt lines row with
          | None -> ()
          | Some line -> LTerm_draw.draw_styled ctx row 0 line
        done
    end
end
