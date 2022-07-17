module type PATH = sig
  val path : string
end

module type ENTRY = sig
  type t

  include Common.Types.STRINGABLE with type t := t
  include Common.Types.SHOWABLE with type t := t
  include Common.Types.SEXPABLE with type t := t
end

module type ABSTRACT_HIST = sig
  type entry

  val get_all : unit -> entry list Lwt.t
  val push : entry -> unit Lwt.t
  val search_from_regex : string -> entry list Lwt.t
  val del_from_regex : string -> unit Lwt.t

  include Common.Types.PPABLE with type t := entry list
end

module type S = ABSTRACT_HIST with type entry := string

module MakeBase (Path : PATH) (Entry : ENTRY) :
  ABSTRACT_HIST with type entry = Entry.t = struct
  type entry = Entry.t

  module Sex = Sexplib
  open Lwt.Syntax

  let get_all () =
    Lwt.catch
      (fun () ->
        let* content =
          Lwt_io.with_file Path.path ~mode:Input Lwt_io.read
        in
        content |> Sex.Sexp.of_string
        |> Sex.Conv.list_of_sexp Entry.t_of_sexp
        |> Lwt.return)
      (function
        | Failure _ | Sex.Sexp.Parse_error _ ->
            let* () = Common.Log.err "History file is corrupted" in
            Lwt.return_nil
        | Unix.Unix_error (ENOENT, _, _) ->
            let* () =
              Lwt_io.with_file Path.path ~mode:Output (fun outc ->
                  Lwt_io.write outc "()")
            in
            let* () = Common.Log.info "Create history file" in
            Lwt.return_nil
        | exn -> Lwt.fail exn)

  let save t =
    Lwt_io.with_file Path.path ~mode:Output (fun outc ->
        let str_sexp =
          Sex.Sexp.to_string @@ Sex.Conv.sexp_of_list Entry.sexp_of_t t
        in
        Lwt_io.write outc str_sexp)

  let push e =
    let* entries = get_all () in
    save (e :: entries)

  let search_from_regex re =
    let regexp = Str.regexp re in
    let* entries = get_all () in
    Lwt.return
    @@ List.filter
         (fun e -> Str.string_match regexp (Entry.to_string e) 0)
         entries

  let del_from_regex re =
    let regexp = Str.regexp re in
    let* entries = get_all () in
    save
    @@ List.filter
         (fun e -> not @@ Str.string_match regexp (Entry.to_string e) 0)
         entries

  let pp () entries = String.concat "\n" @@ List.map Entry.show entries
end

module Make (Path : PATH) : S = struct
  open Sexplib.Conv

  module Entry = struct
    type t = string [@@deriving sexp]

    let show t = t
    let to_string t = t
  end

  include MakeBase (Path) (Entry)
end
