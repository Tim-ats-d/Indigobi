module Back = Back
module Front = Front
module Common = Common
module Gemini = Gemini
module Lib = Lib

module type S = functor
  (ExtHandler : functor (P : Front.Prompt.S) -> Front.ExtHandler.S)
  -> sig
  val main : unit -> unit
end

module Make : S
