module type PPABLE = sig
  type t

  val pp : unit -> t -> string
end

module type SEXPABLE = sig
  type t

  val sexp_of_t : t -> Sexplib.Type.t
  val t_of_sexp : Sexplib.Type.t -> t
end

module type SHOWABLE = sig
  type t

  val show : t -> string
end

module type STRINGABLE = sig
  type t

  val to_string : t -> string
end
