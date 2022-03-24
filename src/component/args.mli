module type S = sig
  type t = {
    mutable address : string list;
    mutable raw : bool;
  }

  val params : t
  val parse_args : (string, Common.Err.t) result
end

module Default : S
