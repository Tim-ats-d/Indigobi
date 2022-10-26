module type S = sig
  val main : unit -> unit
end

module Make : functor (Backend : Back.S) (Printer : Frontend.Printer.S) -> S
