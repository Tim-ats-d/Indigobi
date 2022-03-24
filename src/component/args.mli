module type S = sig
  val usage_msg : Arg.usage_msg
  val address : string list ref
  val raw : bool ref
  val anon_fun : Arg.anon_fun
  val speclist : (Arg.key * Arg.spec * Arg.doc) list
  val parse_args : (string, Common.Err.t) result
end

module Default : S
