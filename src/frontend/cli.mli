type t = {
  mutable address : string option;
  mutable raw_mode : bool;
  mutable certificate : string;
  mutable timeout : float;
}

module type S = sig
  val parse : unit -> (t, [ `BadTimeoutFormat ]) result
end

module Default : S
