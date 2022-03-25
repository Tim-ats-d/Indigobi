type t = {
  mutable address : string option;
  mutable raw : bool;
  mutable certificate : string;
}

module type S = sig
  val parse : unit -> t
end

module Default : S
