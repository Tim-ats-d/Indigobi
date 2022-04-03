type t = Search of search

and search = {
  mutable adresss : string option;
  mutable raw : bool;
  mutable certificate : string;
}

exception UnknownSubCmd of string

module type S = sig
  val parse : unit -> t
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

  let anon_fun = function
    | "search" ->
        sub_cmd := Some `Search;
        speclist := specs_search;
        search.adresss <- Some Sys.argv.(2)
    | other when Sys.argv.(1) <> "search" -> raise @@ UnknownSubCmd other
    | _ -> ()

  let parse () =
    let usage_msg =
      Printf.sprintf "%s [ COMMAND ] [ OPTIONS ]..." Sys.argv.(0)
    in
    (try Arg.parse_dynamic speclist anon_fun usage_msg
     with Invalid_argument _ -> ());
    match !sub_cmd with
    | None ->
        Arg.usage !speclist usage_msg;
        exit 1
    | Some `Search -> Search search
end
