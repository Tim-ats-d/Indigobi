type t = Search of search

and search = {
  mutable adresss : string option;
  mutable raw : bool;
  mutable certificate : string;
}

module type S = sig
  val parse :
    unit -> (t, [> `UnknownSubCmd of string | `Usage of string ]) result
end

module Default : S = struct
  let search = { adresss = None; raw = false; certificate = "" }

  let specs_search =
    [
      ("--raw", Arg.Unit (fun () -> search.raw <- true), "Disable formatting");
      ( "--cert",
        Arg.String (fun c -> search.certificate <- c),
        "Attach client certificate" );
    ]

  let speclist = ref []
  let sub_cmd : [ `Search ] option ref = ref None

  exception UnknownSubCmd of string

  let anon_fun = function
    | "search" ->
        sub_cmd := Some `Search;
        speclist := specs_search;
        search.adresss <- Some Sys.argv.(2)
    | other when Sys.argv.(1) <> "search" ->
        raise_notrace @@ UnknownSubCmd other
    | _ -> ()

  let parse () =
    let usage_msg =
      Printf.sprintf "%s [ COMMAND ] [ OPTIONS ]..." Sys.argv.(0)
    in
    let error_msg () =
      match !sub_cmd with
      | None -> Error (`Usage (Arg.usage_string !speclist usage_msg))
      | Some `Search -> Ok (Search search)
    in
    try
      Arg.parse_dynamic speclist anon_fun usage_msg;
      error_msg ()
    with
    | Invalid_argument _ -> error_msg ()
    | UnknownSubCmd cmd -> Error (`UnknownSubCmd cmd)
end
