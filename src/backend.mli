module type S = sig
  val get :
    url:string ->
    host:string ->
    port:int ->
    cert:string ->
    timeout:float ->
    ( Back.Mime.t * string,
      [> Common.Err.back | Gemini.Status.err ] )
    Lwt_result.t
end

module Make : functor
  (Prompt : Front.Prompt.S)
  (Requester : Back.Requester.S)
  -> S
