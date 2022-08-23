open Sexplib.Std
open Lwt.Syntax

type t = { host : string; fingerprint : string; expiration_date : float }
[@@deriving sexp]

let cache = Common.Accessor.make Cache "known_hosts"

let get (module CacheAcc : Common.Accessor.S) =
  let* content = CacheAcc.read () in
  match content with
  | None ->
      let* () = CacheAcc.write "()" in
      Lwt.return_nil
  | Some c -> c |> Sexplib.Conv.list_of_sexp t_of_sexp |> Lwt.return

let save (module CacheAcc : Common.Accessor.S) entries =
  CacheAcc.write
    Sexplib.(Sexp.to_string @@ Sexplib.Conv.sexp_of_list sexp_of_t entries)

let get_by_host (module CacheAcc : Common.Accessor.S) host =
  let* entries = get (module CacheAcc) in
  List.find_opt (fun e -> String.equal e.host host) entries |> Lwt.return

let save_entry (module CacheAcc : Common.Accessor.S)
    { host; fingerprint; expiration_date } =
  let* entries = get (module CacheAcc) in
  { host; fingerprint; expiration_date }
  :: List.filter (fun e -> e.host <> host) entries
  |> save (module CacheAcc)
