module type S = sig
  val text : LTerm_style.t
  val link : LTerm_style.t
  val visited_link : LTerm_style.t
  val preformat : LTerm_style.t
  val h1 : LTerm_style.t
  val h2 : LTerm_style.t
  val h3 : LTerm_style.t
  val prompt : LTerm_style.t
  val warning : LTerm_style.t
  val error : LTerm_style.t
end

module Default : S
