open Sexplib.Std
open Lwt.Syntax
open Common

type t = { host : string; fingerprint : string; expiration_date : float }
[@@deriving sexp]

let cache = Accessor.make Cache "known_hosts"

let get ~cache:(module CacheAcc : Accessor.S) =
  let* content = CacheAcc.read () in
  match content with
  | None ->
      let* () = CacheAcc.write "()" in
      Lwt.return_nil
  | Some c -> c |> Sexplib.Conv.list_of_sexp t_of_sexp |> Lwt.return

let save ~cache:(module CacheAcc : Accessor.S) entries =
  CacheAcc.write
    Sexplib.(Sexp.to_string @@ Sexplib.Conv.sexp_of_list sexp_of_t entries)

let get_by_host host =
  let* entries = get ~cache in
  Lwt.return @@ List.find_opt (fun e -> String.equal e.host host) entries

let save_entry { host; fingerprint; expiration_date } =
  let* entries = get ~cache in
  { host; fingerprint; expiration_date }
  :: List.filter (fun e -> e.host <> host) entries
  |> save ~cache
