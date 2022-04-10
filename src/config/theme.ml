type style = LTerm_style.t

module type S = sig
  val text : style
  val preformat : style
  val h1 : style
  val h2 : style
  val h3 : style
  val prompt : style
  val warning : style
  val error : style
end

module Default : S = struct
  let text = LTerm_style.{ none with foreground = Some default }
  let preformat = LTerm_style.{ none with foreground = Some Color.yellow }

  let h1 =
    LTerm_style.
      { none with foreground = Some Color.light_red; bold = Some true }

  let h2 = LTerm_style.{ none with foreground = Some Color.dark_green }
  let h3 = LTerm_style.{ none with foreground = Some Color.dark_blue }
  let prompt = LTerm_style.{ none with foreground = Some Color.purple }
  let warning = LTerm_style.{ none with foreground = Some Color.orange }
  let error = LTerm_style.{ none with foreground = Some Color.dark_red }
end
