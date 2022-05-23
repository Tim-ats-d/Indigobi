type t = History of hist | Search of search
and hist = { mutable mode : [ `Del of string | `Display | `Search of string ] }

and search = {
  mutable address : string option;
  mutable raw : bool;
  mutable certificate : string;
}

module type S = sig
  val parse :
    unit -> (t, [> `UnknownSubCmd of string | `Usage of string ]) result
end

module Default : S = struct
  let search = { address = None; raw = false; certificate = "" }
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

  let specs_search =
    [
      ("--raw", Arg.Unit (fun () -> search.raw <- true), "Disable formatting");
      ( "--cert",
        Arg.String (fun c -> search.certificate <- c),
        "Attach client certificate" );
    ]

  let speclist = ref []
  let sub_cmd : [ `History | `Search ] option ref = ref None

  exception UnknownSubCmd of string

  let anon_fun str =
    match !sub_cmd with
    | None -> (
        match str with
        | "search" ->
            sub_cmd := Some `Search;
            speclist := specs_search;
            search.address <- Some Sys.argv.(2)
        | "hist" ->
            sub_cmd := Some `History;
            speclist := specs_hist
        | other when Sys.argv.(1) <> "hist" || Sys.argv.(1) <> "search" ->
            raise_notrace @@ UnknownSubCmd other
        | _ -> ())
    | _ -> ()

  let parse () =
    let usage_msg =
      Printf.sprintf "%s [ COMMAND ] [ OPTIONS ]..." Sys.argv.(0)
    in
    let error_msg () =
      match !sub_cmd with
      | None -> Error (`Usage (Arg.usage_string !speclist usage_msg))
      | Some `History -> Ok (History hist)
      | Some `Search -> Ok (Search search)
    in
    try
      Arg.parse_dynamic speclist anon_fun usage_msg;
      error_msg ()
    with
    | Invalid_argument _ -> error_msg ()
    | UnknownSubCmd cmd -> Error (`UnknownSubCmd cmd)
end
