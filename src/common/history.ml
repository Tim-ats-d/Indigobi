module type ENTRY = sig
  type t

  include Types.STRINGABLE with type t := t
  include Types.SHOWABLE with type t := t
  include Types.SEXPABLE with type t := t
end

type 'a t = (module Accessor.S) * (module ENTRY with type t = 'a)

open Lwt.Syntax

let create (type a) ~fname (module Entry : ENTRY with type t = a) : a t =
  (Accessor.make Cache fname, (module Entry))

let get (type a) (((module CacheAcc), (module Entry)) : a t) =
  Lwt.catch
    (fun () ->
      let* content = CacheAcc.read () in
      match content with
      | None ->
          let* () = CacheAcc.write "()" in
          Lwt.return_nil
      | Some c ->
          c |> Sexplib.Sexp.of_string
          |> Sexplib.Conv.list_of_sexp Entry.t_of_sexp
          |> Lwt.return)
    (function
      | Failure _ | Sexplib.Sexp.Parse_error _ ->
          let* () = Log.err "History file is corrupted" in
          Lwt.return_nil
      | exn -> Lwt.fail exn)

let save (type a) (((module CacheAcc), (module Entry)) : a t) entries =
  CacheAcc.write
    Sexplib.(
      Sexp.to_string @@ Sexplib.Conv.sexp_of_list Entry.sexp_of_t entries)

let push t entry =
  let* entries = get t in
  save t (entry :: entries)

let search_from_regex (type a) (t : a t) re =
  let module Entry = (val snd t) in
  let regexp = Str.regexp re in
  let* entries = get t in
  Lwt.return
  @@ List.filter
       (fun e -> Str.string_match regexp (Entry.to_string e) 0)
       entries

let del_from_regex (type a) (t : a t) re =
  let module Entry = (val snd t) in
  let regexp = Str.regexp re in
  let* entries = get t in
  save t
  @@ List.filter
       (fun e -> not @@ Str.string_match regexp (Entry.to_string e) 0)
       entries

let get_pp_entries (type a) (t : a t) () entries =
  let module Entry = (val snd t) in
  List.map Entry.show entries |> String.concat "\n"
