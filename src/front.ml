open Common

module type S = sig
  val main : unit -> unit
end

module Context = struct
  type t = {
    term : Term.t;
    gemtext : Gemtext.t;
    theme : Theme.t;
    offset_y : int;
    range_y : int;
  }

  let make ~term ~theme ~gemtext =
    { term; gemtext; theme; offset_y = 0; range_y = List.length gemtext }

  let scroll t = function
    | `Up -> { t with offset_y = Int.max 0 (t.offset_y - 1) }
    | `Down -> { t with offset_y = Int.min (t.range_y - 1) (t.offset_y + 1) }
end

module Make (Back : Back.S) (Printer : Frontend.Printer.S) : S = struct
  open Lwt.Syntax

  let rec refresh ctx =
    let img = update ctx in
    let* () = Term.image ctx.Context.term img in
    Lwt.return ctx

  and update { Context.offset_y; range_y; gemtext; theme; _ } =
    List.fold_left
      (fun (i, img) line ->
        ( i + 1,
          if offset_y <= i && i <= range_y then
            Notty.Infix.(img <-> Printer.gemini theme line)
          else img ))
      (0, Img.empty) gemtext
    |> snd

  let loop event ({ Context.term; _ } as ctx) =
    match event with
    | `Key (`ASCII 'q', []) | `Key (`ASCII 'C', [ `Ctrl ]) | `Key (`Escape, [])
      ->
        let* () = Term.release term in
        Lwt.return ctx
    | `Key (`Arrow ((`Up | `Down) as dir), []) ->
        Context.scroll ctx dir |> refresh
    | `Resize _ ->
        let* () = Term.image term (Img.string Attr.empty "Resized") in
        Lwt.return ctx
    | _ -> Lwt.return ctx

  let launch addr ~certificate ~timeout =
    let url = Url.parse addr "" in
    let* cert = Back.cert_from_file certificate in
    match cert with
    | Ok c -> (
        let* result =
          Back.get ~url:(Url.to_string url) ~host:url.domain ~port:url.port
            ~cert:c timeout
        in
        match result with
        | Ok (Gemini, body) ->
            let term = Term.create () in
            let theme =
              Attr.
                {
                  Theme.text = empty;
                  link = st bold ++ fg blue;
                  link_name = st bold;
                  preformat = fg (gray 4);
                  h1 = fg red;
                  h2 = fg green;
                  h3 = fg blue;
                  item = empty;
                  quote = fg (gray 12);
                }
            in
            let* ctx =
              Context.make ~term ~gemtext:(Gemtext.parse body) ~theme |> refresh
            in
            Lwt_stream.fold_s loop (Term.events term) ctx
        | Ok _ -> failwith "todo"
        | Error #Backend.Status.err -> failwith "todo"
        | Error #Err.err -> failwith "todo")
    | Error _ -> failwith "todo"

  let main () =
    try
      launch Sys.argv.(1) ~certificate:"" ~timeout:5.0 |> Lwt_main.run |> ignore
    with Invalid_argument _ -> failwith "todo"
end
