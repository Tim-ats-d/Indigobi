type t = {
  mutable address : string option;
  mutable raw_mode : bool;
  mutable certificate : string;
  mutable timeout : float;
}

module type S = sig
  val parse : unit -> (t, [ `BadTimeoutFormat ]) result
end

module Default : S = struct
  exception BadTimeoutFormat

  let args =
    { address = None; raw_mode = false; certificate = ""; timeout = 5.0 }

  let specs =
    [
      ("--help", Arg.Unit (fun () -> ()), "Display the list of command");
      ( "--raw",
        Arg.Unit (fun () -> args.raw_mode <- true),
        "Disable Gemtext formatting" );
      ( "--cert",
        Arg.String (fun c -> args.certificate <- c),
        "Attach client certificate" );
      ( "--timeout",
        Arg.String
          (fun t ->
            match float_of_string_opt t with
            | None -> raise_notrace BadTimeoutFormat
            | Some t -> args.timeout <- t),
        "Set timeout duration" );
    ]

  let parse () =
    let usage = Printf.sprintf "%s [ COMMAND ] [ OPTIONS ]..." Sys.argv.(0) in
    try
      Arg.parse specs (fun url -> args.address <- Some url) usage;
      Ok args
    with BadTimeoutFormat -> Error `BadTimeoutFormat
end
