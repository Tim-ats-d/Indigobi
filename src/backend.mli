module type S = sig
  val get :
    url:string ->
    host:string ->
    port:int ->
    cert:string ->
    (Component.Mime.t * string, [> Common.Err.t | Gemini.Status.err ]) result
end

module Make : functor
  (Input : Component.Input.S)
  (Requester : Component.Requester.S)
  -> S
