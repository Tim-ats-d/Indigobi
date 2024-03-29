module type ENTRY = sig
  type t

  val equal : t -> t -> bool
  val from_string : string -> t
  val to_string : t -> string
  val show : t -> string
  val sexp_of_t : t -> Sexplib.Type.t
  val t_of_sexp : Sexplib.Type.t -> t
end

type 'a t = (module Accessor.S) * (module ENTRY with type t = 'a)

open Lwt.Syntax

let create (type a) ~fname (module Entry : ENTRY with type t = a) : a t =
  (Accessor.make Cache fname, (module Entry))

let entry (type a) (t : a t) = snd t

let get (type a) (((module CacheAcc), (module Entry)) : a t) =
  let* content = CacheAcc.read () in
  match content with
  | None ->
      let* () = CacheAcc.write "()" in
      Lwt.return_nil
  | Some c -> c |> Sexplib.Conv.list_of_sexp Entry.t_of_sexp |> Lwt.return

let save (type a) (((module CacheAcc), (module Entry)) : a t) entries =
  CacheAcc.write
    Sexplib.(
      Sexp.to_string @@ Sexplib.Conv.sexp_of_list Entry.sexp_of_t entries)

let push t entry =
  let* entries = get t in
  save t (entry :: entries)

let mem (type a) (t : a t) entry =
  let* entries = get t in
  Lwt.return @@ List.mem entry entries

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
  String.concat "\n" @@ List.map Entry.show entries
