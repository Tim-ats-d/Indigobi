module type PATH = sig
  val path : string
end

module type ENTRY = sig
  type t

  include Common.Types.STRINGABLE with type t := t
  include Common.Types.SHOWABLE with type t := t
  include Common.Types.SEXPABLE with type t := t
end

module type BASE_HIST = sig
  type entry

  val add_entry : entry -> unit
  val iter_s : (entry -> unit Lwt.t) -> unit Lwt.t
  val search_from_regex : string -> entry list Lwt.t
end

module MakeBase (Path : PATH) (Entry : ENTRY) :
  BASE_HIST with type entry = Entry.t = struct
  type entry = Entry.t

  module Slib = Sexplib

  let create_hist_file () =
    let oc = open_out Path.path in
    output_string oc "()";
    close_out oc

  let load () =
    try
      Slib.Sexp.load_sexp Path.path |> Slib.Conv.list_of_sexp Entry.t_of_sexp
    with
    | Failure _ -> []
    | Sys_error _ ->
        create_hist_file ();
        []

  let save t =
    Slib.Conv.sexp_of_list Entry.sexp_of_t t |> Slib.Sexp.save_mach Path.path

  let add_entry entry = entry :: load () |> save
  let iter_s f = Lwt_list.iter_s f @@ load ()

  let search_from_regex re =
    let regexp = Str.regexp re in
    Lwt_list.filter_s (fun e ->
        Lwt.return @@ Str.string_match regexp (Entry.to_string e) 0)
    @@ load ()
end

module type S = BASE_HIST with type entry := string

module Make (Path : PATH) : S = struct
  open Sexplib.Conv

  module Entry = struct
    type t = string [@@deriving sexp]

    let show = Fun.id
    let to_string = Fun.id
  end

  include MakeBase (Path) (Entry)
end
