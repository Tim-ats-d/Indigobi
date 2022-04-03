type t = Search of search

and search = {
  mutable adresss : string option;
  mutable raw : bool;
  mutable certificate : string;
}

exception UnknownSubCmd of string

module type S = sig
  val parse : unit -> t
  (** @raise [UnknownSubCmd] *)
end

module Default : S
