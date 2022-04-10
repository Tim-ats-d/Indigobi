module type ENTRY = sig
  type t

  val from_string : string -> t
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t
end

module type S = sig
  type entry

  val add_entry : entry -> unit
  val iter : (entry -> unit) -> unit
end

module Make : functor
  (Path : sig
     val path : string
   end)
  (Entry : ENTRY)
  -> S with type entry = Entry.t

module Default : sig
  module Entry : ENTRY
end
