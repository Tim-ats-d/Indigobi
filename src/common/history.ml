module type ENTRY = sig
  type t

  include Types.STRINGABLE with type t := t
  include Types.SHOWABLE with type t := t
  include Types.SEXPABLE with type t := t
end

type 'a t = { path : string; entry : (module ENTRY with type t = 'a) }

open Lwt.Syntax

let create (type a) ~path (module Entry : ENTRY with type t = a) =
  { path; entry = (module Entry) }

let get (type a) { path; entry = (module Entry : ENTRY with type t = a) } =
  Lwt.catch
    (fun () ->
      let* content = Lwt_io.with_file path ~mode:Input Lwt_io.read in
      content |> Sexplib.Sexp.of_string
      |> Sexplib.Conv.list_of_sexp Entry.t_of_sexp
      |> Lwt.return)
    (function
      | Failure _ | Sexplib.Sexp.Parse_error _ ->
          let* () = Log.err "History file is corrupted" in
          Lwt.return_nil
      | Unix.Unix_error (ENOENT, _, _) ->
          let* () =
            Lwt_io.with_file path ~mode:Output (fun outc ->
                Lwt_io.write outc "()")
          in
          let* () = Log.info "Create history file" in
          Lwt.return_nil
      | exn -> Lwt.fail exn)

let save (type a) { path; entry = (module Entry : ENTRY with type t = a) }
    entries =
  Lwt_io.with_file path ~mode:Output (fun outc ->
      let str_sexp =
        Sexplib.(
          Sexp.to_string @@ Sexplib.Conv.sexp_of_list Entry.sexp_of_t entries)
      in
      Lwt_io.write outc str_sexp)

let push t entry =
  let* entries = get t in
  save t (entry :: entries)

let search_from_regex (type a) t re =
  let module Entry = (val t.entry : ENTRY with type t = a) in
  let regexp = Str.regexp re in
  let* entries = get t in
  Lwt.return
  @@ List.filter
       (fun e -> Str.string_match regexp (Entry.to_string e) 0)
       entries

let del_from_regex (type a) t re =
  let module Entry = (val t.entry : ENTRY with type t = a) in
  let regexp = Str.regexp re in
  let* entries = get t in
  save t
  @@ List.filter
       (fun e -> not @@ Str.string_match regexp (Entry.to_string e) 0)
       entries

let get_pp_entries (type a) t =
  let module Entry = (val t.entry : ENTRY with type t = a) in
  fun () entries -> List.map Entry.show entries |> String.concat "\n"
