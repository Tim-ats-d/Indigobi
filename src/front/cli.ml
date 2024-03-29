type t = History of hist | Search of search
and hist = { mutable mode : [ `Del of string | `Display | `Search of string ] }

and search = {
  mutable address : string option;
  mutable raw_mode : bool;
  mutable certificate : string;
  mutable timeout : float;
}

module type S = sig
  val parse :
    unit ->
    ( t,
      [> `CliErrBadTimeoutFormat
      | `CliErrUnknownSubCmd of string
      | `CliErrUsageMsg of string ] )
    result
end

module Default : S = struct
  let speclist =
    ref [ ("--help", Arg.Unit (fun () -> ()), "Display the list of command") ]

  let hist = { mode = `Display }

  let specs_hist =
    [
      ( "-r",
        Arg.String (fun r -> hist.mode <- `Search r),
        "Print history entries that match regex" );
      ( "-d",
        Arg.String (fun r -> hist.mode <- `Del r),
        "Delete history entries that match regex" );
    ]

  exception BadTimeoutFormat

  let search =
    { address = None; raw_mode = false; certificate = ""; timeout = 5.0 }

  let specs_search =
    [
      ( "--raw",
        Arg.Unit (fun () -> search.raw_mode <- true),
        "Disable Gemtext formatting" );
      ( "--cert",
        Arg.String (fun c -> search.certificate <- c),
        "Attach client certificate" );
      ( "--timeout",
        Arg.String
          (fun t ->
            match float_of_string_opt t with
            | None -> raise_notrace BadTimeoutFormat
            | Some t -> search.timeout <- t),
        "Set timeout duration" );
    ]

  let sub_cmd : [ `History | `Search ] option ref = ref None

  exception UnknownSubCmd of string

  let anon_fun str =
    match !sub_cmd with
    | None when String.equal str "search" ->
        sub_cmd := Some `Search;
        speclist := specs_search;
        search.address <- Some Sys.argv.(2)
    | None when String.equal str "hist" ->
        sub_cmd := Some `History;
        speclist := specs_hist
    | None when not (List.mem Sys.argv.(1) [ "hist"; "search" ]) ->
        raise_notrace @@ UnknownSubCmd str
    | _ -> ()

  let parse () =
    let usage = Printf.sprintf "%s [ COMMAND ] [ OPTIONS ]..." Sys.argv.(0) in

    let error_msg () =
      match !sub_cmd with
      | None ->
          let usage_msg =
            Printf.sprintf
              "%s\n\n\
               Command: hist, search\n\
              \  Type `%s CMD --help` for display the list of command options."
              usage Sys.argv.(0)
          in
          speclist := specs_hist @ specs_search;
          Error (`CliErrUsageMsg usage_msg)
      | Some `History -> Ok (History hist)
      | Some `Search -> Ok (Search search)
    in
    try
      Arg.parse_dynamic speclist anon_fun usage;
      error_msg ()
    with
    | Invalid_argument _ -> error_msg ()
    | BadTimeoutFormat -> Error `CliErrBadTimeoutFormat
    | UnknownSubCmd cmd -> Error (`CliErrUnknownSubCmd cmd)
end
