open Sexplib.Std
open Lwt.Syntax

type t = { host : string; hash : string; expiration_date : int * int }
[@@deriving sexp]

let get (module CacheAcc : Accessor.S) =
  let* content = CacheAcc.read () in
  match content with
  | None ->
      let* () = CacheAcc.write "()" in
      Lwt.return_nil
  | Some c -> c |> Sexplib.Conv.list_of_sexp t_of_sexp |> Lwt.return

let save (module CacheAcc : Accessor.S) entries =
  CacheAcc.write
    Sexplib.(Sexp.to_string @@ Sexplib.Conv.sexp_of_list sexp_of_t entries)

let get_by_host (module CacheAcc : Accessor.S) host =
  let* entries = get (module CacheAcc) in
  List.find_opt (fun e -> String.equal e.host host) entries |> Lwt.return

let save_entry (module CacheAcc : Accessor.S) { host; hash; expiration_date } =
  let* entries = get (module CacheAcc) in
  { host; hash; expiration_date }
  :: List.filter (fun e -> e.host <> host) entries
  |> save (module CacheAcc)
