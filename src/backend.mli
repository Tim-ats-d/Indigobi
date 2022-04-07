module type S = sig
  val get :
    url:string ->
    host:string ->
    port:int ->
    cert:string ->
    (Component.Mime.t * string, [> Common.Err.back | Gemini.Status.err ]) result
end

module Make : functor
  (Prompt : Component.Prompt.S)
  (Requester : Component.Requester.S)
  -> S
