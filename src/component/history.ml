module type ENTRY = sig
  type t

  val from_string : string -> t
  val sexp_of_t : t -> Sexplib.Type.t
  val t_of_sexp : Sexplib.Type.t -> t
end

module type S = sig
  type entry

  val add_entry : entry -> unit
  val iter : (entry -> unit) -> unit
end

module Make (Path : sig
  val path : string
end)
(Entry : ENTRY) : S with type entry = Entry.t = struct
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
  let iter f = List.iter f @@ load ()
end


module Default = struct
  open Sexplib.Conv

  module Entry : ENTRY = struct
    type t = string [@@deriving sexp]

    let from_string = Fun.id
  end
end
