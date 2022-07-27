module type S = sig
  val get_page :
    url:string ->
    host:string ->
    port:int ->
    cert:string ->
    ( Back.Mime.t * string,
      [> Common.Err.back | Gemini.Status.err ] )
    Lwt_result.t
end

module Make : functor
  (Prompt : Front.Prompt.S)
  (Requester : Back.Requester.S)
  -> S
