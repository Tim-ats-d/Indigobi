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

  val get_entries : unit -> entry list
  val add_entry : entry -> unit
  val search_from_regex : string -> entry list
  val del_from_regex : string -> unit

  include Common.Types.SHOWABLE with type t := entry list
end

module MakeBase (Path : PATH) (Entry : ENTRY) :
  BASE_HIST with type entry = Entry.t = struct
  type entry = Entry.t

  module Slib = Sexplib

  let create_hist_file () =
    let oc = open_out Path.path in
    output_string oc "()";
    close_out oc

  let get_entries () =
    try
      Slib.Sexp.load_sexp Path.path |> Slib.Conv.list_of_sexp Entry.t_of_sexp
    with
    | Failure _ -> []
    | Sys_error _ ->
        create_hist_file ();
        []

  let save_entries t =
    Slib.Conv.sexp_of_list Entry.sexp_of_t t |> Slib.Sexp.save_mach Path.path

  let add_entry e = e :: get_entries () |> save_entries

  let search_from_regex re =
    let regexp = Str.regexp re in
    get_entries ()
    |> List.filter (fun e -> Str.string_match regexp (Entry.to_string e) 0)

  let del_from_regex re =
    let regexp = Str.regexp re in
    get_entries ()
    |> List.filter (fun e ->
           not @@ Str.string_match regexp (Entry.to_string e) 0)
    |> save_entries

  let show entries = List.map Entry.show entries |> String.concat "\n"
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
