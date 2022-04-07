type t = Search of search

and search = {
  mutable adresss : string option;
  mutable raw : bool;
  mutable certificate : string;
}

module type S = sig
  val parse :
    unit -> (t, [> `UnknownSubCmd of string | `Usage of string ]) result
end

module Default : S
